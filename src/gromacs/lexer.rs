use std::collections::{HashMap, VecDeque};
use std::mem;
use std::path::{Path, PathBuf};

type Result<T, E = &'static str> = std::result::Result<T, E>;

/// Macro items can be ignored by the parser as expansion happens at lex time
#[allow(single_use_lifetimes)]
#[derive(Debug, PartialEq)]
pub enum Item<'s> {
    Directive(&'s str),
    DataLine(Vec<&'s str>),
    ObjectMacro { name: &'s str, def: &'s str },
    IncludeMacro { text: &'s str, items: GmxLexer<'s> },
    UndefMacro { name: &'s str },
    IfdefMacro(bool),
    EndIfMacro,
    ElseMacro,
}

impl Item<'_> {
    pub fn is_macro(&self) -> bool {
        match self {
            Self::ObjectMacro { .. } => true,
            Self::IncludeMacro { .. } => true,
            Self::IfdefMacro(_) => true,
            Self::UndefMacro { .. } => true,
            Self::ElseMacro => true,
            Self::EndIfMacro => true,
            Self::Directive(_) => false,
            Self::DataLine(_) => false,
        }
    }
}

/// Newtype around std::str::Chars to implement PartialEq
#[derive(Debug, Clone)]
struct Chars<'s>(std::str::Chars<'s>);

impl<'s> Chars<'s> {
    fn as_str(&self) -> &'s str {
        self.0.as_str()
    }
}

impl<'s> Iterator for Chars<'s> {
    type Item = <std::str::Chars<'s> as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl PartialEq for Chars<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<str> for Chars<'_> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

/// A lexer for GROMACS topologies
#[allow(single_use_lifetimes)]
#[derive(Debug, Clone, PartialEq)]
pub struct GmxLexer<'s> {
    iter: Chars<'s>,
    yielded_err: bool,
    macros: HashMap<&'s str, &'s str>,
    current_expansion: VecDeque<&'s str>,
    expansion_is_eof: bool,
    expansion_is_eol: bool,
    current_include: Option<Box<Self>>,
    include_paths: Vec<PathBuf>,
    current_if_true_depth: usize,
    current_if_depth: usize,
}

impl<'s> GmxLexer<'s> {
    /// Create a new lexer from a GROMACS topology as a str
    pub fn new(input: &'s str) -> Self {
        Self::with_macros(input, HashMap::with_capacity(20))
    }

    /// Create a new lexer from a GROMACS topology as a str, with some predefined macros
    pub fn with_macros(input: &'s str, macros: HashMap<&'s str, &'s str>) -> Self {
        Self::with_macros_and_include(input, macros, Vec::with_capacity(20))
    }

    /// Create a new lexer from a GROMACS topology as a str, with some predefined macros
    pub fn with_macros_and_include(
        input: &'s str,
        macros: HashMap<&'s str, &'s str>,
        include_paths: Vec<PathBuf>,
    ) -> Self {
        GmxLexer {
            iter: Chars(input.chars()),
            yielded_err: false,
            macros,
            current_expansion: VecDeque::with_capacity(20),
            expansion_is_eol: false,
            expansion_is_eof: false,
            current_include: None,
            include_paths,
            current_if_true_depth: 0,
            current_if_depth: 0,
        }
    }

    /// Define a macro for the lexer
    pub fn add_macro(mut self, key: &'s str, value: &'s str) -> Self {
        self.macros.insert(key, value);
        self
    }

    /// Define a macro for the lexer
    pub fn add_include_path(mut self, path: impl AsRef<Path>) -> Self {
        self.include_paths.push(PathBuf::from(path.as_ref()));
        self
    }

    fn expand_macros(&mut self, input: &'s str) {
        for word in input.split_whitespace() {
            if let Some(&expanded) = self.macros.get(word) {
                self.expand_macros(expanded);
            } else if word != "" {
                self.current_expansion.push_back(word);
            }
        }
    }

    /// Give the next character, after escaping and ignoring comments
    fn next_char(&mut self) -> Result<Option<char>> {
        match self.iter.next() {
            Some('\\') => match self.iter.next() {
                Some('\n') => Ok(Some(' ')),
                Some(_) => Err("Don't know how to interpret escaped character"),
                None => Err("Unexpected EOF while interpreting escape character"),
            },
            Some(';') => {
                loop {
                    match self.iter.next() {
                        None => break,
                        Some('\n') => break,
                        _ => continue,
                    }
                }
                Ok(Some('\n'))
            }
            Some(c) => Ok(Some(c)),
            None => Ok(None),
        }
    }

    /// Consume leading whitespace, yield a token, then consume a single whitespace character
    ///
    /// Words are composed of any number of non-whitespace characters. EOL or EOF is an error.
    fn next_token_no_expand(&mut self) -> Result<&'s str, (&'s str, &'static str)> {
        assert!(self.current_expansion.is_empty());

        let mut token = self.iter.as_str();
        let mut len = 0;

        loop {
            match self.next_char().map_err(|e| ("", e))? {
                None => return Err((&token[0..len], "EOF")),
                Some('\n') => return Err((&token[0..len], "EOL")),
                Some(c) if c.is_whitespace() && len == 0 => token = self.iter.as_str(),
                Some(c) if c.is_whitespace() => break,
                Some(_) => len += 1,
            };
        }

        Ok(&token[0..len])
    }

    /// Consume leading whitespace, expand macros, yield a token, then consume a single whitespace character
    ///
    /// Words are composed of any number of non-whitespace characters. EOL or EOF is an error.
    fn next_token(&mut self) -> Result<&'s str, (&'s str, &'static str)> {
        while self.current_expansion.is_empty() {
            match self.next_token_no_expand() {
                Ok(token) => self.expand_macros(token),
                Err((token, "EOL")) => {
                    self.expansion_is_eol = true;
                    self.expand_macros(token);
                    break;
                }
                Err((token, "EOF")) => {
                    self.expansion_is_eof = true;
                    self.expand_macros(token);
                    break;
                }
                Err(e) => return Err(e),
            }
        }

        let token = self.current_expansion.pop_front().unwrap_or("");

        if self.current_expansion.is_empty() {
            if self.expansion_is_eof {
                Err((token, "EOF"))
            } else if self.expansion_is_eol {
                self.expansion_is_eol = false;
                Err((token, "EOL"))
            } else {
                Ok(token)
            }
        } else {
            Ok(token)
        }
    }

    fn lex_define_macro(&mut self) -> <Self as Iterator>::Item {
        let (name, def) = match self.next_token_no_expand() {
            Ok(s) if s.contains("(") || s.contains(")") => {
                return Err("Function-like macros not supported")
            }
            Ok(s) => {
                let mut def = self.iter.as_str();
                let mut len = 0;
                loop {
                    match self.next_char()? {
                        None => break,
                        Some('\n') => break,
                        Some(c) if c.is_whitespace() && len == 0 => def = self.iter.as_str(),
                        Some(_) => len += 1,
                    }
                }
                def = &def[0..len];
                (s, def)
            }
            Err((s, e)) if e == "EOF" || e == "EOL" => (s, ""),
            Err((_, e)) => return Err(e),
        };

        self.macros.insert(name, def);

        Ok(Item::ObjectMacro { name, def })
    }

    fn lex_undef_macro(&mut self) -> <Self as Iterator>::Item {
        let name = match self.next_token_no_expand() {
            Ok(s) if s.contains("(") || s.contains(")") => {
                return Err("Function-like macros not supported")
            }
            Ok(s) => loop {
                match self.next_char()? {
                    None => break s,
                    Some('\n') => break s,
                    Some(c) if c.is_whitespace() => continue,
                    Some(c) => {
                        println!("{:?}, {:?}", self.iter.as_str(), c);
                        return Err("Unexpected character after #undef macro");
                    }
                }
            },
            Err((s, e)) if e == "EOF" || e == "EOL" => s,
            Err((_, e)) => return Err(e),
        };

        self.macros.remove(name);

        Ok(Item::UndefMacro { name })
    }

    fn lex_ifdef_macro(&mut self) -> <Self as Iterator>::Item {
        let name = match self.next_token_no_expand() {
            Ok(s) if s.contains("(") || s.contains(")") => {
                return Err("Function-like macros not supported")
            }
            Ok(s) => loop {
                match self.next_char()? {
                    None => break s,
                    Some('\n') => break s,
                    Some(c) if c.is_whitespace() => continue,
                    Some(c) => {
                        println!("{:?}, {:?}", self.iter.as_str(), c);
                        return Err("Unexpected character after #undef macro");
                    }
                }
            },
            Err((s, e)) if e == "EOF" || e == "EOL" => s,
            Err((_, e)) => return Err(e),
        };

        if self.macros.contains_key(name) {
            Ok(Item::IfdefMacro(true))
        } else {
            Ok(Item::IfdefMacro(false))
        }
    }

    fn lex_endif_macro(&mut self, gobble_whitespace: bool) -> <Self as Iterator>::Item {
        if gobble_whitespace {
            loop {
                match self.next_char()? {
                    None => break,
                    Some('\n') => break,
                    Some(c) if c.is_whitespace() => continue,
                    Some(c) => {
                        println!("{:?}, {:?}", self.iter.as_str(), c);
                        return Err("Unexpected character after #endif macro");
                    }
                }
            }
        }
        Ok(Item::EndIfMacro)
    }

    fn lex_else_macro(&mut self, gobble_whitespace: bool) -> <Self as Iterator>::Item {
        if gobble_whitespace {
            loop {
                match self.next_char()? {
                    None => break,
                    Some('\n') => break,
                    Some(c) if c.is_whitespace() => continue,
                    Some(c) => {
                        println!("{:?}, {:?}", self.iter.as_str(), c);
                        return Err("Unexpected character after #else macro");
                    }
                }
            }
        }
        Ok(Item::ElseMacro)
    }

    fn lex_include_macro(&mut self) -> <Self as Iterator>::Item {
        let mut gobble_whitespace = true;
        let path = match self.next_token_no_expand() {
            Ok(s) if s.starts_with("\"") && s.ends_with("\"") => &s[1..s.len() - 1],
            Ok(s) if s.starts_with("\"") => {
                return Err("whitespace-containing quoted paths are not supported")
            }
            Ok(s) => s,
            Err(("", e)) if e == "EOF" => return Err("Unexpected EOF in #include declaration"),
            Err(("", e)) if e == "EOL" => return Err("Unexpected EOL in #include declaration"),
            Err((s, e))
                if (e == "EOF" || e == "EOL") && s.starts_with("\"") && s.ends_with("\"") =>
            {
                gobble_whitespace = false;
                &s[1..s.len() - 1]
            }
            Err((s, e)) if (e == "EOF" || e == "EOL") && s.starts_with("\"") => {
                return Err("whitespace-containing quoted paths are not supported");
            }
            Err((s, e)) if e == "EOF" || e == "EOL" => {
                gobble_whitespace = false;
                s
            }
            Err((_, e)) => return Err(e),
        };

        if gobble_whitespace {
            match self.next_token_no_expand() {
                Ok("") => Ok(()),
                Err(("", e)) if e == "EOF" || e == "EOL" => Ok(()),
                Ok(_) => Err("Unexpected character after #include declaration"),
                Err((_, e)) if e == "EOF" || e == "EOL" => {
                    Err("Unexpected character after #include declaration")
                }
                Err((_, e)) => Err(e),
            }?;
        }

        // Read the path and leak the text to produce an allocated 'static str
        // We'll hold onto a pointer to it so we can deallocate it in unsafe code
        // in a wrapper function that reads the topology file, lexes and parses it,
        // and then cleans up
        // TODO: Figure out how to use Pin rather than a leak
        use std::io::Read as _;
        let mut text = String::new();
        let mut file = std::fs::File::open(path);
        let mut path_used = PathBuf::from(".");
        for include_path in &self.include_paths {
            if file.is_ok() {
                break;
            }
            let mut include_path = include_path.to_path_buf();
            include_path.push(path);
            if let &Some(p) = &include_path.parent() {
                path_used = p.to_owned()
            }
            file = std::fs::File::open(&include_path);
        }

        file.map_err(|_| "Error opening #include file")?
            .read_to_string(&mut text)
            .map_err(|_| "Error reading #include file")?;
        let text: &'s str = Box::leak(text.into_boxed_str());

        let items = GmxLexer::with_macros_and_include(
            text,
            self.macros.clone(),
            self.include_paths.clone(),
        )
        .add_include_path(path_used);

        Ok(Item::IncludeMacro { text, items })
    }

    // Lex a macro
    fn lex_macro(&mut self) -> <Self as Iterator>::Item {
        match self.next_token() {
            Ok("define") => self.lex_define_macro(),
            Ok("include") => self.lex_include_macro(),
            Ok("undef") => self.lex_undef_macro(),
            Ok("ifdef") => self.lex_ifdef_macro(),
            Ok("if") => Err("#if macros are unimplemented"),
            Ok("else") => self.lex_else_macro(true),
            Err(("else", "EOL")) | Err(("else", "EOF")) => self.lex_else_macro(false),
            Ok("elif") => Err("#elif macros are unimplemented"),
            Ok("endif") => self.lex_endif_macro(true),
            Err(("endif", "EOL")) | Err(("endif", "EOF")) => self.lex_endif_macro(false),
            Ok(_) => Err("Unknown macro declaration"),
            Err((_, "EOF")) => Err("End of file in middle of macro definition"),
            Err((_, "EOL")) => Err("End of line in middle of macro definition"),
            Err((_, e)) => Err(e),
        }
    }

    /// Lex a directive
    fn lex_directive(&mut self) -> <Self as Iterator>::Item {
        let name = match self.next_token() {
            Err((s, "EOF")) | Err((s, "EOL")) if s.ends_with(']') => {
                return Ok(Item::Directive(&s[0..s.len() - 1]))
            }
            Ok(s) => s,
            Err((_, "EOF")) => {
                return Err("Unexpected end of file while lexing a directive header")
            }
            Err((_, "EOL")) => {
                return Err("Unexpected end of line while lexing a directive header")
            }
            Err((_, e)) => return Err(e),
        };

        // Now we need to make sure that the line ends appropriately - with a ] if there hasn't
        // been one already, and otherwise with only whitespace
        let expected = if name.ends_with("]") { "" } else { "]" };

        match self.next_token() {
            Ok(s) if s == expected => match self.next_token() {
                Err(("", "EOF")) | Err(("", "EOL")) => Ok(Item::Directive(name)),
                Ok(_) | Err((_, "EOF")) | Err((_, "EOL")) => {
                    Err("Unexpected character after directive header")
                }
                Err((_, e)) => Err(e),
            },
            Ok(_) => Err("Unexpected character in directive header"),
            Err((s, "EOF")) | Err((s, "EOL")) if s == expected => Ok(Item::Directive(name)),
            Err((_, "EOF")) => Err("Unexpected end of file while lexing a directive header"),
            Err((_, "EOL")) => {
                return Err("Unexpected end of line while lexing a directive header")
            }
            Err((_, e)) => Err(e),
        }
    }

    /// Lex a line of data
    fn lex_data(&mut self) -> <Self as Iterator>::Item {
        let mut fields = Vec::with_capacity(20);

        loop {
            match self.next_token() {
                Ok(s) => fields.push(s),
                Err((s, "EOF")) | Err((s, "EOL")) => {
                    if s != "" {
                        fields.push(s);
                    }
                    break;
                }
                Err((_, e)) => return Err(e),
            }
        }

        Ok(Item::DataLine(fields))
    }

    fn next_item(&mut self) -> Option<<Self as Iterator>::Item> {
        //Match the first character of the next item to decide what to do next
        let prev_iter = self.iter.clone();
        match self.next_char() {
            Ok(opt) => match opt {
                Some('#') => Some(self.lex_macro()),
                Some('[') => Some(self.lex_directive()),
                Some(c) if c.is_whitespace() => self.next(),
                Some(_) => {
                    // lex_data actually needs the first character, so we go back a step before
                    // calling it
                    self.iter = prev_iter;
                    Some(self.lex_data())
                }
                None => None,
            },
            Err(e) => Some(Err(e)),
        }
    }

    /// Consume the lexer, returning an error if one occurred or otherwise returning self
    pub fn lex_all(mut self) -> Result<Self> {
        loop {
            match self.next() {
                Some(Ok(_)) => continue,
                Some(Err(e)) => return Err(e),
                None => break,
            }
        }
        Ok(self)
    }

    fn current_if(&self) -> bool {
        self.current_if_depth == self.current_if_true_depth
    }
}

impl<'s> Iterator for GmxLexer<'s> {
    type Item = Result<Item<'s>>;

    fn next(&mut self) -> Option<Self::Item> {
        // If last time yielded an error, we're done
        if self.yielded_err {
            return None;
        }

        // Check if we're in the middle of a #include
        if let Some(lexer) = self.current_include.as_mut() {
            match lexer.next() {
                Some(result) => return Some(result),
                None => {
                    mem::swap(&mut lexer.macros, &mut self.macros);
                    self.current_include = None;
                }
            }
        }

        let next_item = self.next_item();

        if self.current_if() {
            match next_item {
                Some(Ok(Item::IncludeMacro { items, .. })) => {
                    self.current_include = Some(Box::new(items));
                    self.next()
                }
                Some(Ok(Item::IfdefMacro(true))) => {
                    self.current_if_depth += 1;
                    self.current_if_true_depth += 1;
                    self.next()
                }
                Some(Ok(Item::IfdefMacro(false))) => {
                    self.current_if_depth += 1;
                    self.next()
                }
                Some(Ok(Item::ElseMacro)) if self.current_if_depth == 0 => {
                    return Some(Err("Unmatched #else"));
                }
                Some(Ok(Item::ElseMacro)) => {
                    self.current_if_true_depth -= 1;
                    self.next()
                }
                Some(Ok(Item::EndIfMacro)) if self.current_if_depth == 0 => {
                    return Some(Err("Unmatched #endif"));
                }
                Some(Ok(Item::EndIfMacro)) => {
                    self.current_if_depth -= 1;
                    self.current_if_true_depth -= 1;
                    self.next()
                }
                Some(Ok(t)) if t.is_macro() => self.next(),
                Some(Ok(t)) => Some(Ok(t)),
                Some(Err(e)) => {
                    self.yielded_err = true;
                    Some(Err(e))
                }
                None => None,
            }
        } else {
            match next_item {
                Some(Ok(Item::IncludeMacro { .. })) => self.next(),
                Some(Ok(Item::IfdefMacro(_))) => {
                    self.current_if_depth += 1;
                    self.next()
                }
                Some(Ok(Item::EndIfMacro)) => {
                    self.current_if_depth -= 1;
                    self.next()
                }
                Some(Ok(Item::ElseMacro))
                    if self.current_if_true_depth + 1 == self.current_if_depth =>
                {
                    self.current_if_true_depth += 1;
                    self.next()
                }
                Some(Ok(_)) => self.next(),
                Some(Err(e)) => {
                    self.yielded_err = true;
                    Some(Err(e))
                }
                None => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct RawItemIter<'s>(GmxLexer<'s>);

    impl<'s> Iterator for RawItemIter<'s> {
        type Item = <GmxLexer<'s> as Iterator>::Item;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next_item()
        }
    }

    #[test]
    fn test_next_token() {
        let input = "hello there buddy";
        let mut lexer = GmxLexer::new(input);
        let mut output = Vec::with_capacity(20);
        loop {
            let result = lexer.next_token().clone();
            output.push(result);
            if let Err((_, "EOF")) = result {
                break;
            }
        }
        let expected = vec![Ok("hello"), Ok("there"), Err(("buddy", "EOF"))];
        assert_eq!(output, expected);
    }

    #[test]
    fn test_lex_define() {
        let input = "#define POSRES";
        let output: Result<Vec<Item<'static>>, _> = RawItemIter(GmxLexer::new(input)).collect();
        let expected = Ok(vec![Item::ObjectMacro {
            name: "POSRES",
            def: "",
        }]);
        assert_eq!(output, expected);

        let input = "#define POSRESFC 10000";
        let output: Result<Vec<Item<'static>>, _> = RawItemIter(GmxLexer::new(input)).collect();
        let expected = Ok(vec![Item::ObjectMacro {
            name: "POSRESFC",
            def: "10000",
        }]);
        assert_eq!(output, expected);

        let input = "#define POSRESFC() 10000";
        let output: Result<Vec<Item<'static>>, _> = RawItemIter(GmxLexer::new(input)).collect();
        let expected = Err("Function-like macros not supported");
        assert_eq!(output, expected);

        let input = "#define POSRESFC  10000 10000    10000 ; this is a comment\n";
        let output: Result<Vec<Item<'static>>, _> = RawItemIter(GmxLexer::new(input)).collect();
        let expected = Ok(vec![Item::ObjectMacro {
            name: "POSRESFC",
            def: "10000 10000    10000 ",
        }]);
        assert_eq!(output, expected);

        let input = "#define POSRES\n#define POSRESFC 10000";
        let output: Result<Vec<Item<'static>>, _> = RawItemIter(GmxLexer::new(input)).collect();
        let expected = Ok(vec![
            Item::ObjectMacro {
                name: "POSRES",
                def: "",
            },
            Item::ObjectMacro {
                name: "POSRESFC",
                def: "10000",
            },
        ]);
        assert_eq!(output, expected);
    }

    #[test]
    fn test_ifdef() -> Result<()> {
        let input = "
            #define POSRES
            #define POSRESFC 10000

            #ifdef POSRES
            [ position_restraints ]
            0  POSRESFC POSRESFC POSRESFC
            2  POSRESFC POSRESFC POSRESFC
            3  POSRESFC POSRESFC POSRESFC
            6  POSRESFC POSRESFC POSRESFC
            8  POSRESFC POSRESFC POSRESFC
            #ifdef POSRES_LIGHT
            1  POSRESFC POSRESFC POSRESFC
            4  POSRESFC POSRESFC POSRESFC
            5  POSRESFC POSRESFC POSRESFC
            7  POSRESFC POSRESFC POSRESFC
            #endif
            #endif
        ";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Item::Directive("position_restraints"),
            Item::DataLine(vec!["0", "10000", "10000", "10000"]),
            Item::DataLine(vec!["2", "10000", "10000", "10000"]),
            Item::DataLine(vec!["3", "10000", "10000", "10000"]),
            Item::DataLine(vec!["6", "10000", "10000", "10000"]),
            Item::DataLine(vec!["8", "10000", "10000", "10000"]),
        ];

        assert_eq!(output.len(), expected.len());
        for (o, e) in output.iter().zip(expected.iter()) {
            assert_eq!(o, e);
        }
        Ok(())
    }

    #[test]
    fn test_else() -> Result<()> {
        let input = "
            #define POSRES
            #define POSRESFC 10000

            #ifdef POSRES
            [ position_restraints ]
            0  POSRESFC POSRESFC POSRESFC
            2  POSRESFC POSRESFC POSRESFC
            3  POSRESFC POSRESFC POSRESFC
            6  POSRESFC POSRESFC POSRESFC
            8  POSRESFC POSRESFC POSRESFC
            #ifdef POSRES_LIGHT
            1  POSRESFC POSRESFC POSRESFC
            4  POSRESFC POSRESFC POSRESFC
            5  POSRESFC POSRESFC POSRESFC
            7  POSRESFC POSRESFC POSRESFC
            #else
            1  0.000000 0.000000 0.000000
            4  0.000000 0.000000 0.000000
            5  0.000000 0.000000 0.000000
            7  0.000000 0.000000 0.000000
            #endif
            9 POSRESFC POSRESFC POSRESFC
            #endif
        ";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Item::Directive("position_restraints"),
            Item::DataLine(vec!["0", "10000", "10000", "10000"]),
            Item::DataLine(vec!["2", "10000", "10000", "10000"]),
            Item::DataLine(vec!["3", "10000", "10000", "10000"]),
            Item::DataLine(vec!["6", "10000", "10000", "10000"]),
            Item::DataLine(vec!["8", "10000", "10000", "10000"]),
            Item::DataLine(vec!["1", "0.000000", "0.000000", "0.000000"]),
            Item::DataLine(vec!["4", "0.000000", "0.000000", "0.000000"]),
            Item::DataLine(vec!["5", "0.000000", "0.000000", "0.000000"]),
            Item::DataLine(vec!["7", "0.000000", "0.000000", "0.000000"]),
            Item::DataLine(vec!["9", "10000", "10000", "10000"]),
        ];

        for (o, e) in output.iter().zip(expected.iter()) {
            assert_eq!(o, e);
        }
        assert_eq!(output.len(), expected.len());
        Ok(())
    }

    #[test]
    fn test_lex_included() -> Result<()> {
        let macros = "
            #define POSRES position_restraints
            #define POSRESFC 10000
            #define POSRESFC3 10000 10000 10000
            #define POSRESFC2 POSRESFC POSRESFC
        ";
        let macros = GmxLexer::new(macros).lex_all()?.macros;

        let input = "
            [ POSRES ]
            0  POSRESFC POSRESFC POSRESFC
            1  POSRESFC3  ; Keep the trailing whitespace
            2  POSRESFC2 POSRESFC
            #define POSRESFC 5000
            3  POSRESFC2 POSRESFC
            4  10000 10000 10000
        ";
        let included = GmxLexer::with_macros(input, macros.clone());

        let outer = GmxLexer {
            iter: Chars("5 POSRESFC2 POSRESFC".chars()),
            yielded_err: false,
            macros: macros.clone(),
            current_expansion: VecDeque::new(),
            expansion_is_eof: false,
            expansion_is_eol: false,
            current_include: Some(Box::new(included)),
            include_paths: Vec::with_capacity(5),
            current_if_true_depth: 0,
            current_if_depth: 0,
        };

        let output: Result<Vec<Item<'static>>, _> = outer.collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Item::Directive("position_restraints"),
            Item::DataLine(vec!["0", "10000", "10000", "10000"]),
            Item::DataLine(vec!["1", "10000", "10000", "10000"]),
            Item::DataLine(vec!["2", "10000", "10000", "10000"]),
            Item::DataLine(vec!["3", "5000", "5000", "5000"]),
            Item::DataLine(vec!["4", "10000", "10000", "10000"]),
            Item::DataLine(vec!["5", "5000", "5000", "5000"]),
        ];

        assert_eq!(output.len(), expected.len());
        for (o, e) in output.iter().zip(expected.iter()) {
            assert_eq!(o, e);
        }

        Ok(())
    }

    #[test]
    fn test_expand_macros() -> Result<()> {
        let input = "
            #define POSRES position_restraints
            [ POSRES ]
        ";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        #[rustfmt::skip]
        let expected = Ok(vec![
            Item::Directive("position_restraints"),
        ]);
        assert_eq!(output, expected);

        let input = "
            #define POSRES position_restraints
            #define POSRESFC 10000
            #define POSRESFC3 10000 10000 10000
            #define POSRESFC2 POSRESFC POSRESFC

            [ POSRES ]
            0  POSRESFC POSRESFC POSRESFC
            1  POSRESFC3  ; Keep the trailing whitespace
            2  POSRESFC2 POSRESFC
            #define POSRESFC 5000
            3  POSRESFC2 POSRESFC
            4  10000 10000 10000
            #undef POSRESFC
            5  POSRESFC2 POSRESFC
            6  POSRESFC3
        ";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Item::Directive("position_restraints"),
            Item::DataLine(vec!["0", "10000", "10000", "10000"]),
            Item::DataLine(vec!["1", "10000", "10000", "10000"]),
            Item::DataLine(vec!["2", "10000", "10000", "10000"]),
            Item::DataLine(vec!["3", "5000", "5000", "5000"]),
            Item::DataLine(vec!["4", "10000", "10000", "10000"]),
            Item::DataLine(vec!["5", "POSRESFC", "POSRESFC", "POSRESFC"]),
            Item::DataLine(vec!["6", "10000", "10000", "10000"]),
        ];

        assert_eq!(output.len(), expected.len());
        for (o, e) in output.iter().zip(expected.iter()) {
            assert_eq!(o, e);
        }
        Ok(())
    }

    #[test]
    fn test_lex_comment() {
        let input = "; Comments are ignored by the lexer";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![]);
        assert_eq!(output, expected);
    }

    #[test]
    fn test_lex_directive() {
        let input = "[ defaults ]";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![Item::Directive("defaults")]);
        assert_eq!(output, expected);

        let input = "[position_restraints]";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![Item::Directive("position_restraints")]);
        assert_eq!(output, expected);

        let input = "[ defa ults ]";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected character in directive header");
        assert_eq!(output, expected);

        let input = "[ defaults \n ]";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected end of line while lexing a directive header");
        assert_eq!(output, expected);

        let input = "[ defaults ] hello!";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected character after directive header");
        assert_eq!(output, expected);

        let input = "[ defaults hello! ] ";
        let output: Result<Vec<Item<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected character in directive header");
        assert_eq!(output, expected);
    }

    #[test]
    fn test_lex_data() -> Result<()> {
        let input = "
            Br          35      79.90    0.0000  A   0.00000e+00  0.00000e+00
            C            6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            CA           6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            F            9      19.00    0.0000  A   3.11815e-01  2.55224e-01
            HA           1       1.008   0.0000  A   2.59964e-01  6.27600e-02
            Cl          17      35.45    0.0000  A   4.40104e-01  4.18400e-01
            Na          11      22.99    0.0000  A   3.32840e-01  1.15897e-02
        ";
        let output: Result<Vec<Item<'static>>> = GmxLexer::new(input).collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Item::DataLine(vec!["Br", "35", "79.90", "0.0000", "A", "0.00000e+00", "0.00000e+00"]),
            Item::DataLine(vec![ "C",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Item::DataLine(vec!["CA",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Item::DataLine(vec![ "F",  "9", "19.00", "0.0000", "A", "3.11815e-01", "2.55224e-01"]),
            Item::DataLine(vec!["HA",  "1", "1.008", "0.0000", "A", "2.59964e-01", "6.27600e-02"]),
            Item::DataLine(vec!["Cl", "17", "35.45", "0.0000", "A", "4.40104e-01", "4.18400e-01"]),
            Item::DataLine(vec!["Na", "11", "22.99", "0.0000", "A", "3.32840e-01", "1.15897e-02"]),
        ];

        assert_eq!(output.len(), expected.len());
        for (o, e) in output.iter().zip(expected.iter()) {
            assert_eq!(o, e);
        }
        Ok(())
    }

    #[test]
    fn test_lex_topology() -> Result<()> {
        let input = r"
            #define H_MASS 1.008

            [ defaults ]
            ; nb   combo  pairs   fudge_lj  fudge_qq
            1       2     yes     0.5  0.8333

            [ atomtypes ]
            ; name      at.num  mass     charge ptype  sigma      epsilon
            Br          35      79.90    0.0000  A   0.00000e+00  0.00000e+00
            C            6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            CA           6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            F            9      19.00    0.0000  A   3.11815e-01  2.55224e-01
            HA           1      H_MASS   0.0000  A   2.59964e-01  6.27600e-02
            Cl          17      35.45    0.0000  A   4.40104e-01  4.18400e-01
            Na          11      22.99    0.0000  A   3.32840e-01  1.15897e-02

            [ bondtypes ]
             Br  CA      1    0.18900  143929.6  ; Amber99Sb-disp
             C   C       1    0.15250  259408.0  ; Amber99Sb-disp
             C   CA      1    0.14090  392459.2  ; Amber99Sb-disp
             F   CA      1    0.13590  323004.8  ; Amber99Sb-disp
             CA  HA      1    0.10800  307105.6  ; Amber99Sb-disp
             Cl  CA      1    0.17270  161502.4  ; Amber99Sb-disp

            [ cmaptypes ]

            C NH1 CT1 C NH1 1 5 5\
            0.53048936 3.21624080 4.06375184 5.23405848 8.87430584\
            4.18872792 -9.20697568 -20.19897128 -20.17293425 -20.55692503\
            -9.77110440 -6.37079270 -3.98125173 -0.15334360 -0.53192447\
            10.19537874 8.07122888 4.54573190 2.69198560 1.08230038\
            -9.61955155 -6.28119234 -4.60984752 -3.60423986 -2.67846291


        ";
        let output = GmxLexer::new(input).collect::<Result<Vec<_>>>()?;

        #[rustfmt::skip]
        let expected = vec![
            Item::Directive("defaults"),
            Item::DataLine(vec!["1", "2", "yes", "0.5", "0.8333"]),
            Item::Directive("atomtypes"),
            Item::DataLine(vec!["Br", "35", "79.90", "0.0000", "A", "0.00000e+00", "0.00000e+00"]),
            Item::DataLine(vec![ "C",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Item::DataLine(vec!["CA",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Item::DataLine(vec![ "F",  "9", "19.00", "0.0000", "A", "3.11815e-01", "2.55224e-01"]),
            Item::DataLine(vec!["HA",  "1", "1.008", "0.0000", "A", "2.59964e-01", "6.27600e-02"]),
            Item::DataLine(vec!["Cl", "17", "35.45", "0.0000", "A", "4.40104e-01", "4.18400e-01"]),
            Item::DataLine(vec!["Na", "11", "22.99", "0.0000", "A", "3.32840e-01", "1.15897e-02"]),
            Item::Directive("bondtypes"),
            Item::DataLine(vec!["Br", "CA", "1", "0.18900", "143929.6"]),
            Item::DataLine(vec![ "C",  "C", "1", "0.15250", "259408.0"]),
            Item::DataLine(vec![ "C", "CA", "1", "0.14090", "392459.2"]),
            Item::DataLine(vec![ "F", "CA", "1", "0.13590", "323004.8"]),
            Item::DataLine(vec!["CA", "HA", "1", "0.10800", "307105.6"]),
            Item::DataLine(vec!["Cl", "CA", "1", "0.17270", "161502.4"]),
            Item::Directive("cmaptypes"),
            Item::DataLine(vec![
                "C", "NH1", "CT1", "C", "NH1", "1", "5", "5",
                "0.53048936",  "3.21624080", "4.06375184", "5.23405848", "8.87430584",
                "4.18872792", "-9.20697568", "-20.19897128", "-20.17293425", "-20.55692503",
                "-9.77110440", "-6.37079270", "-3.98125173", "-0.15334360", "-0.53192447",
                "10.19537874", "8.07122888", "4.54573190", "2.69198560", "1.08230038",
                "-9.61955155", "-6.28119234", "-4.60984752", "-3.60423986", "-2.67846291"
            ]),
        ];

        assert_eq!(output.len(), expected.len());
        for (o, e) in output.iter().zip(expected.iter()) {
            assert_eq!(o, e);
        }
        Ok(())
    }
}
