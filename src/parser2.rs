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
        self.builder
            .start_node(NailLanguage::kind_to_raw(SyntaxKind::Root));
        self.builder.finish_node();

        Parse {
            green_node: self.builder.finish(),
        }
    }
}

pub(crate) struct Parse {
    green_node: GreenNode,
}
