use std::collections::{HashMap, VecDeque};
use std::mem;

type Result<T, E = &'static str> = std::result::Result<T, E>;

/// Macro tokens can be ignored by the parser as expansion happens at lex time
#[allow(single_use_lifetimes)]
#[derive(Debug, PartialEq)]
pub enum Token<'s> {
    Directive(&'s str),
    DataLine(Vec<&'s str>),
    ObjectMacro { name: &'s str, def: &'s str },
    IncludeMacro { text: &'s str, tokens: GmxLexer<'s> },
}

impl Token<'_> {
    pub fn is_macro(&self) -> bool {
        match self {
            Self::ObjectMacro { .. } => true,
            Self::IncludeMacro { .. } => true,
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
}

impl<'s> GmxLexer<'s> {
    /// Create a new lexer from a GROMACS topology as a str
    pub fn new(input: &'s str) -> Self {
        Self::with_macros(input, HashMap::with_capacity(20))
    }

    /// Create a new lexer from a GROMACS topology as a str, with some predefined macros
    pub fn with_macros(input: &'s str, macros: HashMap<&'s str, &'s str>) -> Self {
        GmxLexer {
            iter: Chars(input.chars()),
            yielded_err: false,
            macros,
            current_expansion: VecDeque::with_capacity(20),
            expansion_is_eol: false,
            expansion_is_eof: false,
            current_include: None,
        }
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

    /// Consume leading whitespace, yield a word, then consume a single whitespace character
    ///
    /// Words are composed of any number of non-whitespace characters. EOL or EOF is an error.
    fn next_word_no_expand(&mut self) -> Result<&'s str, (&'s str, &'static str)> {
        assert!(self.current_expansion.is_empty());

        let mut word = self.iter.as_str();
        let mut len = 0;

        loop {
            match self.next_char().map_err(|e| ("", e))? {
                None => return Err((&word[0..len], "EOF")),
                Some('\n') => return Err((&word[0..len], "EOL")),
                Some(c) if c.is_whitespace() && len == 0 => word = self.iter.as_str(),
                Some(c) if c.is_whitespace() => break,
                Some(_) => len += 1,
            };
        }

        Ok(&word[0..len])
    }

    /// Consume leading whitespace, expand macros, yield a word, then consume a single whitespace character
    ///
    /// Words are composed of any number of non-whitespace characters. EOL or EOF is an error.
    fn next_word(&mut self) -> Result<&'s str, (&'s str, &'static str)> {
        while self.current_expansion.is_empty() {
            match self.next_word_no_expand() {
                Ok(word) => self.expand_macros(word),
                Err((word, "EOL")) => {
                    self.expansion_is_eol = true;
                    self.expand_macros(word);
                    break;
                }
                Err((word, "EOF")) => {
                    self.expansion_is_eof = true;
                    self.expand_macros(word);
                    break;
                }
                Err(e) => return Err(e),
            }
        }

        let word = self.current_expansion.pop_front().unwrap_or("");

        if self.current_expansion.is_empty() {
            if self.expansion_is_eof {
                Err((word, "EOF"))
            } else if self.expansion_is_eol {
                self.expansion_is_eol = false;
                Err((word, "EOL"))
            } else {
                Ok(word)
            }
        } else {
            Ok(word)
        }
    }

    fn lex_define_macro(&mut self) -> <Self as Iterator>::Item {
        let name = match self.next_word_no_expand() {
            Ok(s) if s.contains("(") || s.contains(")") => {
                return Err("Function-like macros not supported")
            }
            Ok(s) => s,
            Err((s, e)) if e == "EOF" || e == "EOL" => s,
            Err((_, e)) => return Err(e),
        };

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

        self.macros.insert(name, def);

        Ok(Token::ObjectMacro { name, def })
    }

    fn lex_include_macro(&mut self) -> <Self as Iterator>::Item {
        let path = match self.next_word_no_expand() {
            Ok(s) => s,
            Err(("", e)) if e == "EOF" => return Err("Unexpected EOF in #include declaration"),
            Err(("", e)) if e == "EOL" => return Err("Unexpected EOL in #include declaration"),
            Err((s, e)) if e == "EOF" || e == "EOL" => s,
            Err((_, e)) => return Err(e),
        };

        match self.next_word_no_expand() {
            Ok("") => Ok(()),
            Err(("", e)) if e == "EOF" || e == "EOL" => Ok(()),
            Ok(_) => Err("Unexpected character after #include declaration"),
            Err((_, e)) if e == "EOF" || e == "EOL" => {
                Err("Unexpected character after #include declaration")
            }
            Err((_, e)) => Err(e),
        }?;

        // Read the path and leak the text to produce an allocated 'static str
        // We'll hold onto a pointer to it so we can deallocate it in unsafe code
        // in a wrapper function that reads the topology file, lexes and parses it,
        // and then cleans up
        // TODO: Figure out how to use Pin rather than a leak
        let mut text = String::new();
        use std::io::Read as _;
        std::fs::File::open(path)
            .map_err(|_| "Error opening #include file")?
            .read_to_string(&mut text)
            .map_err(|_| "Error reading #include file")?;
        let text: &'s str = Box::leak(text.into_boxed_str());

        let tokens = GmxLexer::with_macros(text, self.macros.clone());

        Ok(Token::IncludeMacro { text, tokens })
    }

    // Lex a macro
    fn lex_macro(&mut self) -> <Self as Iterator>::Item {
        match self.next_word() {
            Ok("define") => self.lex_define_macro(),
            Ok("include") => self.lex_include_macro(),
            Ok("undef") => Err("#undef macros are unimplemented"),
            Ok("ifdef") => Err("#ifdef macros are unimplemented"),
            Ok("if") => Err("#if macros are unimplemented"),
            Ok("else") => Err("#else macros are unimplemented"),
            Ok("elif") => Err("#elif macros are unimplemented"),
            Ok("endif") => Err("#endif macros are unimplemented"),
            Ok(_) => Err("Unknown macro declaration"),
            Err((_, "EOF")) => Err("End of file in middle of macro definition"),
            Err((_, "EOL")) => Err("End of line in middle of macro definition"),
            Err((_, e)) => Err(e),
        }
    }

    /// Lex a directive
    fn lex_directive(&mut self) -> <Self as Iterator>::Item {
        let name = match self.next_word() {
            Err((s, "EOF")) | Err((s, "EOL")) if s.ends_with(']') => {
                return Ok(Token::Directive(&s[0..s.len() - 1]))
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

        match self.next_word() {
            Ok(s) if s == expected => match self.next_word() {
                Err(("", "EOF")) | Err(("", "EOL")) => Ok(Token::Directive(name)),
                Ok(_) | Err((_, "EOF")) | Err((_, "EOL")) => {
                    Err("Unexpected character after directive header")
                }
                Err((_, e)) => Err(e),
            },
            Ok(_) => Err("Unexpected character in directive header"),
            Err((s, "EOF")) | Err((s, "EOL")) if s == expected => Ok(Token::Directive(name)),
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
            match self.next_word() {
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

        Ok(Token::DataLine(fields))
    }

    fn next_token(&mut self) -> Option<<Self as Iterator>::Item> {
        //Match the first character of the next token to decide what to do next
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
}

impl<'s> Iterator for GmxLexer<'s> {
    type Item = Result<Token<'s>>;

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

        match self.next_token() {
            Some(Ok(Token::IncludeMacro { tokens, .. })) => {
                self.current_include = Some(Box::new(tokens));
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct RawTokenIter<'s>(GmxLexer<'s>);

    impl<'s> Iterator for RawTokenIter<'s> {
        type Item = <GmxLexer<'s> as Iterator>::Item;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next_token()
        }
    }

    #[test]
    fn test_next_word() {
        let input = "hello there buddy";
        let mut lexer = GmxLexer::new(input);
        let mut output = Vec::with_capacity(20);
        loop {
            let result = lexer.next_word().clone();
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
        let input = "#define pos_res_fc 10000";
        let output: Result<Vec<Token<'static>>, _> = RawTokenIter(GmxLexer::new(input)).collect();
        let expected = Ok(vec![Token::ObjectMacro {
            name: "pos_res_fc",
            def: "10000",
        }]);
        assert_eq!(output, expected);

        let input = "#define pos_res_fc() 10000";
        let output: Result<Vec<Token<'static>>, _> = RawTokenIter(GmxLexer::new(input)).collect();
        let expected = Err("Function-like macros not supported");
        assert_eq!(output, expected);

        let input = "#define pos_res_fc  10000 10000    10000 ; this is a comment\n";
        let output: Result<Vec<Token<'static>>, _> = RawTokenIter(GmxLexer::new(input)).collect();
        let expected = Ok(vec![Token::ObjectMacro {
            name: "pos_res_fc",
            def: "10000 10000    10000 ",
        }]);
        assert_eq!(output, expected);
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
        };

        let output: Result<Vec<Token<'static>>, _> = outer.collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Token::Directive("position_restraints"),
            Token::DataLine(vec!["0", "10000", "10000", "10000"]),
            Token::DataLine(vec!["1", "10000", "10000", "10000"]),
            Token::DataLine(vec!["2", "10000", "10000", "10000"]),
            Token::DataLine(vec!["3", "5000", "5000", "5000"]),
            Token::DataLine(vec!["4", "10000", "10000", "10000"]),
            Token::DataLine(vec!["5", "5000", "5000", "5000"]),
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
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        #[rustfmt::skip]
        let expected = Ok(vec![
            Token::Directive("position_restraints"),
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
        ";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Token::Directive("position_restraints"),
            Token::DataLine(vec!["0", "10000", "10000", "10000"]),
            Token::DataLine(vec!["1", "10000", "10000", "10000"]),
            Token::DataLine(vec!["2", "10000", "10000", "10000"]),
            Token::DataLine(vec!["3", "5000", "5000", "5000"]),
            Token::DataLine(vec!["4", "10000", "10000", "10000"]),
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
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![]);
        assert_eq!(output, expected);
    }

    #[test]
    fn test_lex_directive() {
        let input = "[ defaults ]";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![Token::Directive("defaults")]);
        assert_eq!(output, expected);

        let input = "[position_restraints]";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![Token::Directive("position_restraints")]);
        assert_eq!(output, expected);

        let input = "[ defa ults ]";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected character in directive header");
        assert_eq!(output, expected);

        let input = "[ defaults \n ]";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected end of line while lexing a directive header");
        assert_eq!(output, expected);

        let input = "[ defaults ] hello!";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected character after directive header");
        assert_eq!(output, expected);

        let input = "[ defaults hello! ] ";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
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
        let output: Result<Vec<Token<'static>>> = GmxLexer::new(input).collect();
        let output = output?;
        #[rustfmt::skip]
        let expected = vec![
            Token::DataLine(vec!["Br", "35", "79.90", "0.0000", "A", "0.00000e+00", "0.00000e+00"]),
            Token::DataLine(vec![ "C",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Token::DataLine(vec!["CA",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Token::DataLine(vec![ "F",  "9", "19.00", "0.0000", "A", "3.11815e-01", "2.55224e-01"]),
            Token::DataLine(vec!["HA",  "1", "1.008", "0.0000", "A", "2.59964e-01", "6.27600e-02"]),
            Token::DataLine(vec!["Cl", "17", "35.45", "0.0000", "A", "4.40104e-01", "4.18400e-01"]),
            Token::DataLine(vec!["Na", "11", "22.99", "0.0000", "A", "3.32840e-01", "1.15897e-02"]),
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
            [ defaults ]
            ; nb   combo  pairs   fudge_lj  fudge_qq
            1       2     yes     0.5  0.8333

            [ atomtypes ]
            ; name      at.num  mass     charge ptype  sigma      epsilon
            Br          35      79.90    0.0000  A   0.00000e+00  0.00000e+00
            C            6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            CA           6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            F            9      19.00    0.0000  A   3.11815e-01  2.55224e-01
            HA           1       1.008   0.0000  A   2.59964e-01  6.27600e-02
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
            Token::Directive("defaults"),
            Token::DataLine(vec!["1", "2", "yes", "0.5", "0.8333"]),
            Token::Directive("atomtypes"),
            Token::DataLine(vec!["Br", "35", "79.90", "0.0000", "A", "0.00000e+00", "0.00000e+00"]),
            Token::DataLine(vec![ "C",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Token::DataLine(vec!["CA",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Token::DataLine(vec![ "F",  "9", "19.00", "0.0000", "A", "3.11815e-01", "2.55224e-01"]),
            Token::DataLine(vec!["HA",  "1", "1.008", "0.0000", "A", "2.59964e-01", "6.27600e-02"]),
            Token::DataLine(vec!["Cl", "17", "35.45", "0.0000", "A", "4.40104e-01", "4.18400e-01"]),
            Token::DataLine(vec!["Na", "11", "22.99", "0.0000", "A", "3.32840e-01", "1.15897e-02"]),
            Token::Directive("bondtypes"),
            Token::DataLine(vec!["Br", "CA", "1", "0.18900", "143929.6"]),
            Token::DataLine(vec![ "C",  "C", "1", "0.15250", "259408.0"]),
            Token::DataLine(vec![ "C", "CA", "1", "0.14090", "392459.2"]),
            Token::DataLine(vec![ "F", "CA", "1", "0.13590", "323004.8"]),
            Token::DataLine(vec!["CA", "HA", "1", "0.10800", "307105.6"]),
            Token::DataLine(vec!["Cl", "CA", "1", "0.17270", "161502.4"]),
            Token::Directive("cmaptypes"),
            Token::DataLine(vec![
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
