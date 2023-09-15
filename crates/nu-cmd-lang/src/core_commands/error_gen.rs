use nu_engine::CallExt;
use nu_protocol::ast::{Call, Expression};
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
                "Variable or expression to highlight",
            )
            .required(
                "text",
                SyntaxShape::String,
                "Explanatory text under highlight",
            )
            .switch(
                "unspanned",
                "remove the origin label from the error",
                Some('u'),
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
        let unspanned = call.has_flag("unspanned");

        let throw_error = if unspanned { None } else { Some(span) };
        Err(
            make_error(&title, blame, &text, throw_error).unwrap_or_else(|| {
                ShellError::GenericError(
                    "Creating error value not supported.".into(),
                    "unsupported error format".into(),
                    Some(span),
                    None,
                    Vec::new(),
                )
            }),
        )
    }

    fn examples(&self) -> Vec<Example> {
        vec![
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
            Example {
                description: "Create a more complex custom error",
                example: r#"error make {
        msg: "my custom error message"
        label: {
            text: "my custom label text"  # not mandatory unless $.label exists
            start: 123  # not mandatory unless $.label.end is set
            end: 456  # not mandatory unless $.label.start is set
        }
    }"#,
                result: Some(Value::error(
                    ShellError::GenericError(
                        "my custom error message".to_string(),
                        "my custom label text".to_string(),
                        Some(Span::new(123, 456)),
                        None,
                        Vec::new(),
                    ),
                    Span::unknown(),
                )),
            },
            Example {
                description:
                    "Create a custom error for a custom command that shows the span of the argument",
                example: r#"def foo [x] {
        let span = (metadata $x).span;
        error make {
            msg: "this is fishy"
            label: {
                text: "fish right here"
                start: $span.start
                end: $span.end
            }
        }
    }"#,
                result: None,
            },
        ]
    }
}

fn make_error(
    title: &Value,
    blame: &Expression,
    text: &Value,
    _throw_span: Option<Span>,
) -> Option<ShellError> {
    match (title, blame, text) {
        (Value::String { val: title_str, .. }, blame, Value::String { val: text_str, .. }) => {
            Some(ShellError::GenericError(
                title_str.into(),
                text_str.into(),
                Some(blame.span),
                None,
                Vec::new(),
            ))
        }

        _ => None,
    }
}
