use ariadne::{Label, Report, ReportKind, Source};
use clap::Command;
use colorful::Colorful;
use lento_core::{
    interpreter::error::RuntimeError,
    lexer::lexer::InputSource,
    parser::error::ParseError,
    type_checker::checker::TypeError,
    util::error::{BaseError, BaseErrorExt},
};

pub fn print_parse_error(err: ParseError, content: &str, source: &InputSource) {
    print_error_report("parse error", err.to_base(), content, source);
}

pub fn print_runtime_error(err: RuntimeError, content: &str, source: &InputSource) {
    print_error_report("runtime error", err.to_base(), content, source);
}

pub fn print_type_error(err: TypeError, content: &str, source: &InputSource) {
    print_error_report("type error", err.to_base(), content, source);
}

pub fn print_error_report(kind: &str, base: BaseError, content: &str, source: &InputSource) {
    let mut colors = ariadne::ColorGenerator::new();
    let mut report = Report::build(
        ReportKind::Custom(kind, ariadne::Color::BrightRed),
        (source.name(), base.info.start.index..base.info.end.index),
    )
    .with_message(&base.message);

    if base.labels.is_empty() && !base.info.end.eof {
        report = report.with_label(
            Label::new((source.name(), base.info.start.index..base.info.end.index))
                .with_message(base.message)
                .with_color(ariadne::Color::BrightRed),
        );
    }

    for label in base.labels {
        report = report.with_label(
            Label::new((source.name(), label.info.start.index..label.info.end.index))
                .with_message(label.message)
                .with_color(colors.next()),
        );
    }

    if let Some(hint) = base.hint {
        report = report.with_help(hint);
    }

    if base.info.end.eof {
        report = report.with_label(
            Label::new((source.name(), base.info.start.index..base.info.end.index))
                .with_message(format!("end of {}", source.human_readable()))
                .with_color(ariadne::Color::Yellow),
        );
    }

    report
        .finish()
        .print((source.name(), Source::from(content)))
        .unwrap();
}

pub fn print_error_simple(kind: &str, msg: String) {
    println!("{}: {}", kind.light_red(), msg);
}

pub fn print_error(msg: String) {
    print_error_simple("error", msg)
}

pub fn print_error_usage(msg: String, arg_parser: &mut Command) {
    print_error(msg);
    println!("{}\n", arg_parser.render_usage());
    println!("For more information try {}", "--help".bold().yellow())
}
