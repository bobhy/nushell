use nu_engine::{eval_expression, CallExt};
use nu_protocol::ast::{Call, Expr, Expression, FullCellPath};
use nu_protocol::engine::{Command, EngineState, Stack};
use nu_protocol::{
    Category, Example, PipelineData, ShellError, Signature, Span, SyntaxShape, Type, Value,
};

#[derive(Clone)]
pub struct ErrorGen;

impl Command for ErrorGen {
    fn name(&self) -> &str {
        "error gen"
    }

    //future: consider making blame and text optional, so `error <title>` generates an unspanned error
    fn signature(&self) -> Signature {
        Signature::build("error gen")
            .input_output_types(vec![(Type::Nothing, Type::Error)])
            .required("title", SyntaxShape::String, "Title for error")
            .required(
                "blame",
                SyntaxShape::Any,
                "Variable or expression whose span will be highlighted",
            )
            .required(
                "text",
                SyntaxShape::String,
                "Explanatory (label) text under highlight",
            )
            .category(Category::Core)
    }

    fn usage(&self) -> &str {
        "Display an error related to a highlighted item."
    }

    fn search_terms(&self) -> Vec<&str> {
        vec!["panic", "crash", "throw"]
    }

    fn run(
        &self,
        engine_state: &EngineState,
        stack: &mut Stack,
        call: &Call,
        _input: PipelineData,
    ) -> Result<PipelineData, ShellError> {
        let span = call.head;
        let title: Value = call.req(engine_state, stack, 0)?;
        let blame = call
            .positional_nth(1)
            .expect("2nd param is required in signature");
        let text: Value = call.req(engine_state, stack, 2)?;

        let blame_span = blamable_span(engine_state, stack, blame);

        Err(make_error(&title, blame_span, &text).unwrap_or_else(|| {
            ShellError::GenericError(
                "Creating error value not supported.".into(),
                "unsupported error format".into(),
                Some(span),
                None,
                Vec::new(),
            )
        }))
    }

    fn examples(&self) -> Vec<Example> {
        vec![
            /* not supported yet 
            Example {
                description: "Create a simple custom error",
                example: r#"error make {msg: "my custom error message"}"#,
                result: Some(Value::error(
                    ShellError::GenericError(
                        "my custom error message".to_string(),
                        "".to_string(),
                        None,
                        None,
                        Vec::new(),
                    ),
                    Span::unknown(),
                )),
            },
            */
            Example {
                description: "Custom error highlighting bad value or expression",
                example: r#"let bogon = "bogus value"; error gen "my custom error message" $bogon "this value is bogus""#,
                result: Some(Value::error(
                    ShellError::GenericError(
                        "my custom error message".to_string(),
                        "this value is bogus".to_string(),
                        Some(Span::new(123, 456)),
                        None,
                        Vec::new(),
                    ),
                    Span::unknown(),
                )),
            },
        ]
    }
}

// provides span of expression as close to source of error as possible
fn blamable_span(engine_state: &EngineState, stack: &mut Stack, e: &Expression) -> Span {
    let ret_val = match &e.expr {
        Expr::FullCellPath(cell) => {
            match **cell {
                FullCellPath {
                    head:
                        Expression {
                            expr: Expr::Var(varid),
                            span,
                            ..
                        },
                    ..
                } => {
                    if let Ok(vi) = stack.get_var_info(varid, e.span) {
                        //todo what happens here?
                        if let Some(vspan) = vi.span {
                            vspan
                        } else {
                            span
                        }
                    } else {
                        e.span
                    }
                }
                _ => e.span,
            }
        }
        _ => {
            if let Ok(v) = eval_expression(engine_state, stack, e) {
                v.span()
            } else {
                e.span
            }
        }
    };

    //todo eprintln!("blamable expr: {:?}, returned span {:?}", e, ret_val);
    ret_val
}

fn make_error(title: &Value, span: Span, text: &Value) -> Option<ShellError> {
    match (title, span, text) {
        (Value::String { val: title_str, .. }, span, Value::String { val: text_str, .. }) => {
            Some(ShellError::GenericError(
                title_str.into(),
                text_str.into(),
                Some(span),
                None,
                Vec::new(),
            ))
        }

        _ => None,
    }
}
