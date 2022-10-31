use crate::{lexer2::SyntaxKind, syntax::NailLanguage};
use logos::Logos;
use rowan::{GreenNode, GreenNodeBuilder, Language};

pub(crate) struct Parser<'a> {
    lexer: logos::Lexer<'a, SyntaxKind>,
    builder: GreenNodeBuilder<'static>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            lexer: SyntaxKind::lexer(input),
            builder: GreenNodeBuilder::new(),
        }
    }

    pub(crate) fn parse(mut self) -> Parse {
        self.start_node(SyntaxKind::Root);
        self.finish_node();

        Parse {
            green_node: self.builder.finish(),
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(NailLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }
}

pub(crate) struct Parse {
    green_node: GreenNode,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::SyntaxNode;

    #[test]
    fn parse_nothing() {
        let parse = Parser::new("").parse();

        assert_eq!(
            format!("{:#?}", SyntaxNode::new_root(parse.green_node)),
            r#"Root@0..0
"#,
        );
    }
}
