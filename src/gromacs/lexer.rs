type Result<T, E = &'static str> = std::result::Result<T, E>;

#[allow(single_use_lifetimes)]
#[derive(Debug, PartialEq)]
pub enum Token<'s> {
    ObjectMacro { name: &'s str, def: &'s str },
    Directive(&'s str),
    DataLine(Vec<&'s str>),
}

#[derive(Debug, Clone)]
pub struct GmxLexer<'s> {
    iter: std::str::Chars<'s>,
    yielded_err: bool,
}

impl<'s> GmxLexer<'s> {
    /// Create a new lexer from a GROMACS topology as a str
    pub fn new(input: &'s str) -> Self {
        GmxLexer {
            iter: input.chars(),
            yielded_err: false,
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
    fn next_word(&mut self) -> Result<&'s str, (&'s str, &'static str)> {
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

    fn lex_define_macro(&mut self) -> <Self as Iterator>::Item {
        let name = match self.next_word() {
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

        Ok(Token::ObjectMacro {
            name,
            def: &def[0..len],
        })
    }

    // Lex a macro
    fn lex_macro(&mut self) -> <Self as Iterator>::Item {
        match self.next_word() {
            Ok("define") => self.lex_define_macro(),
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
        let mut slice = self.iter.as_str();
        let mut name = None;
        let mut len = 0;

        loop {
            match self.next_char()? {
                Some('\n') => return Err("Unexpected newline while lexing a directive"),
                Some(']') => {
                    if name.is_none() && len > 0 {
                        name = Some(&slice[0..len]);
                    }
                    break;
                }
                Some(c) if c.is_whitespace() && len == 0 => slice = self.iter.as_str(),
                Some(c) if c.is_whitespace() && name.is_none() => name = Some(&slice[0..len]),
                Some(c) if c.is_whitespace() => continue,
                Some(_) if name.is_none() => len += 1,
                Some(_) => return Err("Non-whitespace character after directive name"),
                None => return Err("Unexpected EOF while lexing a directive"),
            };
        }

        // TODO: Check if there should always be a newline after a directive header
        loop {
            match self.next_char()? {
                None => break,
                Some('\n') => break,
                Some(c) if c.is_whitespace() => continue,
                Some(_) => return Err("Unexpected character on directive header line"),
            };
        }

        let name = name.ok_or("No name in header title")?;
        Ok(Token::Directive(name))
    }

    /// Lex a line of data
    fn lex_data(&mut self) -> <Self as Iterator>::Item {
        let mut fields = Vec::with_capacity(20);

        let mut this_field_slice = self.iter.as_str();
        let mut this_field_length = 0;
        loop {
            match self.next_char()? {
                None => break,
                Some('\n') => break,
                Some(c) if c.is_whitespace() => {
                    if this_field_length > 0 {
                        fields.push(&this_field_slice[0..this_field_length]);
                        this_field_length = 0;
                    }
                    this_field_slice = self.iter.as_str();
                }
                Some(_) => this_field_length += 1,
            }
        }

        if this_field_length > 0 {
            fields.push(&this_field_slice[0..this_field_length]);
        }

        Ok(Token::DataLine(fields))
    }
}

impl<'s> Iterator for GmxLexer<'s> {
    type Item = Result<Token<'s>>;

    fn next(&mut self) -> Option<Self::Item> {
        // If last time yielded an error, we're done
        if self.yielded_err {
            return None;
        }

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
            Err(e) => {
                self.yielded_err = true;
                Some(Err(e))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_word() {
        let input = "hello there buddy";
        let mut lexer = GmxLexer::new(input);
        let mut output = Vec::with_capacity(20);
        loop {
            let result = lexer.next_word().clone();
            output.push(result);
            if result.map_err(|(_, e)| e) == Err("EOF") {
                break;
            }
        }
        let expected = vec![Ok("hello"), Ok("there"), Err(("buddy", "EOF"))];
        assert_eq!(output, expected);
    }

    #[test]
    fn test_lex_define() {
        let input = "#define pos_res_fc 10000";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![Token::ObjectMacro {
            name: "pos_res_fc",
            def: "10000",
        }]);
        assert_eq!(output, expected);

        let input = "#define pos_res_fc() 10000";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Function-like macros not supported");
        assert_eq!(output, expected);

        let input = "#define pos_res_fc  10000 10000    10000 ; this is a comment\n";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Ok(vec![Token::ObjectMacro {
            name: "pos_res_fc",
            def: "10000 10000    10000 ",
        }]);
        assert_eq!(output, expected);
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
        let expected = Err("Non-whitespace character after directive name");
        assert_eq!(output, expected);

        let input = "[ defaults \n ]";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected newline while lexing a directive");
        assert_eq!(output, expected);

        let input = "[ defaults ] hello!";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        let expected = Err("Unexpected character on directive header line");
        assert_eq!(output, expected);
    }

    #[test]
    fn test_lex_data() {
        let input = "
            Br          35      79.90    0.0000  A   0.00000e+00  0.00000e+00
            C            6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            CA           6      12.01    0.0000  A   3.39967e-01  3.59824e-01
            F            9      19.00    0.0000  A   3.11815e-01  2.55224e-01
            HA           1       1.008   0.0000  A   2.59964e-01  6.27600e-02
            Cl          17      35.45    0.0000  A   4.40104e-01  4.18400e-01
            Na          11      22.99    0.0000  A   3.32840e-01  1.15897e-02
        ";
        let output: Result<Vec<Token<'static>>, _> = GmxLexer::new(input).collect();
        #[rustfmt::skip]
        let expected = Ok(vec![
            Token::DataLine(vec!["Br", "35", "79.90", "0.0000", "A", "0.00000e+00", "0.00000e+00"]),
            Token::DataLine(vec![ "C",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Token::DataLine(vec!["CA",  "6", "12.01", "0.0000", "A", "3.39967e-01", "3.59824e-01"]),
            Token::DataLine(vec![ "F",  "9", "19.00", "0.0000", "A", "3.11815e-01", "2.55224e-01"]),
            Token::DataLine(vec!["HA",  "1", "1.008", "0.0000", "A", "2.59964e-01", "6.27600e-02"]),
            Token::DataLine(vec!["Cl", "17", "35.45", "0.0000", "A", "4.40104e-01", "4.18400e-01"]),
            Token::DataLine(vec!["Na", "11", "22.99", "0.0000", "A", "3.32840e-01", "1.15897e-02"]),
        ]);
        assert_eq!(output, expected);
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
