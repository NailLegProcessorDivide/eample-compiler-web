#![allow(non_snake_case)]
use std::collections::HashSet;

// import the prelude to get access to the `rsx!` macro and the `Scope` and `Element` types
use dioxus::prelude::*;
use sample_compiler_lib::{
    block_structure::{CFGEntry, Var},
    compile, compile_function,
    live_var_analysis::CFGAnnot,
    source_ast::{self, Func, Prog, Stmt},
    tokens::{self, Token},
    type_check,
};

mod ast_renderer;
mod cfg_renderer;

#[derive(Props)]
struct InputProgram<'a> {
    input_program: &'a str,
}

#[derive(Props)]
struct RenderProgram<'a> {
    input_program: &'a str,
    goal: ViewMode,
}

#[derive(Props)]
struct RenderCompiler<'a> {
    func: &'a Option<Func>,
    globals: &'a Option<HashSet<Var>>,
    goal: ViewMode,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum ViewMode {
    Lexer,
    Parser,
    TypeCheck,
    FuncAST,
    ConstProp,
    UnnestAST,
    InitCFG,
    CleanCFG,
    CFGAnnot,
    UnWrite,
}

fn token_colour(tok: &Token) -> String {
    match tok {
        Token::Num(_) => "green",
        Token::Ident(_) => "orange",
        Token::Op(_) | Token::Uop(_) => "fuchsia",
        _ => "blue",
    }
    .to_string()
}

fn CompilerView<'a>(cx: Scope<'a, RenderCompiler<'a>>) -> Element {
    let stmts: &UseState<Vec<Stmt>> = use_state(cx, || Vec::new());
    let cfg: &UseState<Vec<CFGEntry>> = use_state(cx, || Vec::new());
    let cfg_annot: &UseState<Vec<(CFGEntry, CFGAnnot)>> = use_state(cx, || Vec::new());
    let view_goal = cx.props.goal;
    if view_goal == ViewMode::FuncAST {
        let loc_stmts = compile_function::build_ast(cx.props.func.as_ref().unwrap());
        if &loc_stmts != stmts.get() {
            stmts.set(loc_stmts);
        }
        let render_stmts = stmts
            .get()
            .iter()
            .map(|s| rsx! {ast_renderer::RenderStmt {stmt: s, indent: 1}});
        return cx.render(rsx! {
            pre {
                render_stmts
            }
        });
    }
    if view_goal == ViewMode::ConstProp {
        let loc_stmts = compile_function::prop_stmts(cx.props.func.as_ref().unwrap());
        if &loc_stmts != stmts.get() {
            stmts.set(loc_stmts);
        }
        let render_stmts = stmts
            .get()
            .iter()
            .map(|s| rsx! {ast_renderer::RenderStmt {stmt: s, indent: 1}});
        return cx.render(rsx! {
            pre {
                render_stmts
            }
        });
    }
    if view_goal == ViewMode::UnnestAST {
        let loc_stmts = compile_function::unnest_stmts(cx.props.func.as_ref().unwrap());
        if &loc_stmts != stmts.get() {
            stmts.set(loc_stmts);
        }
        let render_stmts = stmts
            .get()
            .iter()
            .map(|s| rsx! {ast_renderer::RenderStmt {stmt: s, indent: 1}});
        return cx.render(rsx! {
            pre {
                render_stmts
            }
        });
    }
    if view_goal == ViewMode::InitCFG {
        let loc_cfg = compile_function::build_cfg(cx.props.func.as_ref().unwrap());
        if &loc_cfg != cfg.get() {
            cfg.set(loc_cfg);
        }
        return cx.render(rsx! {
            div {
                cfg_renderer::RenderCFG {cfg: cfg.get()}
            }
        });
    }
    if view_goal == ViewMode::CleanCFG {
        let loc_cfg = compile_function::clean_cfg(cx.props.func.as_ref().unwrap());
        if &loc_cfg != cfg.get() {
            cfg.set(loc_cfg);
        }
        return cx.render(rsx! {
            div {
                cfg_renderer::RenderCFG {cfg: cfg.get()}
            }
        });
    }
    if view_goal == ViewMode::CFGAnnot {
        let loc_cfg = compile_function::build_annot_cfg(
            cx.props.globals.as_ref().unwrap(),
            cx.props.func.as_ref().unwrap(),
        );
        if &loc_cfg != cfg_annot.get() {
            cfg_annot.set(loc_cfg);
        }
        return cx.render(rsx! {
            div {
                cfg_renderer::RenderAnnotCFG {cfg: cfg_annot.get()}
            }
        });
    }
    if view_goal == ViewMode::UnWrite {
        let loc_cfg = compile_function::remove_unused_writes(
            cx.props.globals.as_ref().unwrap(),
            cx.props.func.as_ref().unwrap(),
        );
        if &loc_cfg != cfg_annot.get() {
            cfg_annot.set(loc_cfg);
        }
        return cx.render(rsx! {
            div {
                cfg_renderer::RenderAnnotCFG {cfg: cfg_annot.get()}
            }
        });
    }
    return cx.render(rsx! {
        pre {
        }
    });
}

fn FrontEndView<'a>(cx: Scope<'a, RenderProgram<'a>>) -> Element {
    let view_goal = cx.props.goal;
    let comp_view: &UseState<Option<Prog>> = use_state(cx, || None);
    let function: &UseState<Option<Func>> = use_state(cx, || None);
    let globals: &UseState<Option<HashSet<Var>>> = use_state(cx, || None);
    let function_sel = use_state(cx, || 0);
    let toks = match tokens::lex(cx.props.input_program, 0, 0) {
        Ok(toks) => toks,
        Err(err) => {
            return cx.render(rsx! {
                pre { err }
            })
        }
    };
    if view_goal == ViewMode::Lexer {
        let mut prev_loc = 0;
        let mut tokens = Vec::new();
        for tok in toks {
            tokens.push((
                (if prev_loc != tok.loc { "\n" } else { "" }).to_string()
                    + &tokens::show_token(&tok.tok),
                token_colour(&tok.tok),
            ));
            prev_loc = tok.loc;
        }
        let f_tokens = tokens
            .iter()
            .map(|(t, col)| rsx! { span { style : "background-color:{col}", t.clone()} " " });

        return cx.render(rsx! {
            div {
                pre { f_tokens }
            }
        });
    }
    let mut prog = match source_ast::parse_program(&toks) {
        Ok(prog) => prog,
        Err(err) => {
            return cx.render(rsx! {
                pre { err }
            })
        }
    };
    if view_goal == ViewMode::Parser {
        if let Some(cv) = comp_view.get() {
            if *cv != prog {
                comp_view.set(Some(prog));
            }
            return cx.render(rsx! {
                ast_renderer::RenderProg {inp: cv}
            });
        } else {
            comp_view.set(Some(prog));
            return cx.render(rsx! {div {}});
        }
    }
    if let Some(err) = type_check::type_prog(&mut prog) {
        return cx.render(rsx! {
            pre { err }
        });
    }
    if let Some(cv) = comp_view.get() {
        if *cv != prog {
            comp_view.set(Some(prog));
            return render!(rsx! {div{}});
        }
    }
    if view_goal == ViewMode::TypeCheck {
        if let Some(cv) = comp_view.get() {
            return cx.render(rsx! {
                ast_renderer::RenderProg {inp: cv}
            });
        } else {
            comp_view.set(Some(prog));
            return cx.render(rsx! {div {}});
        }
    }
    if comp_view.get().as_ref().unwrap().funcs.first() == None {
        return cx.render(rsx! {
            div { "no func" }
        });
    }

    let loc_func = comp_view.get().as_ref().unwrap().funcs.first().cloned();
    if &loc_func != function.get() {
        function.set(loc_func);
    }

    let loc_globals: HashSet<Var> = HashSet::from_iter(
        prog.globals
            .iter()
            .map(|glob| compile::id_to_var(&glob.var_name)),
    );
    if &Some(loc_globals.clone()) != globals.get() {
        globals.set(Some(loc_globals));
    }
    //let globals : HashSet<Var> = HashSet::from_iter(prog.globals.iter().map(|glob| compile::id_to_var(&glob.var_name)));
    if function.get().is_some() && globals.get().is_some() {
        return cx.render(rsx! {
            div { "choose func" }
            div { CompilerView {func: function.get(), globals: globals.get(), goal: view_goal} }
        });
    } else {
        return cx.render(rsx! {
            div { "choose func" }
        });
    }
}

fn CompilerSelect<'a>(cx: Scope<'a, InputProgram<'a>>) -> Element {
    let comp_view = use_state(cx, || ViewMode::Parser);

    cx.render(rsx! {
        div {
            input {
                r#type : "radio",
                id : "lex",
                name : "comp_view",
                value : "lexer",
                onclick : move |_| comp_view.set(ViewMode::Lexer)
            }
            label {
                r#for : "lex",
                "lexer"
            }
            input {
                r#type : "radio",
                id : "parse",
                name : "comp_view",
                value : "parse",
                onclick : move |_| comp_view.set(ViewMode::Parser)
            }
            label {
                r#for : "parse",
                "parser"
            }
            input {
                r#type : "radio",
                id : "type",
                name : "comp_view",
                value : "type",
                onclick : move |_| comp_view.set(ViewMode::TypeCheck)
            }
            label {
                r#for : "type",
                "type check"
            }
            input {
                r#type : "radio",
                id : "func_ast",
                name : "comp_view",
                value : "func_ast",
                onclick : move |_| comp_view.set(ViewMode::FuncAST)
            }
            label {
                r#for : "func_ast",
                "func ast"
            }
            input {
                r#type : "radio",
                id : "const_prop",
                name : "comp_view",
                value : "const_prop",
                onclick : move |_| comp_view.set(ViewMode::ConstProp)
            }
            label {
                r#for : "const_prop",
                "const prop"
            }
            input {
                r#type : "radio",
                id : "unnest_ast",
                name : "comp_view",
                value : "unnest_ast",
                onclick : move |_| comp_view.set(ViewMode::UnnestAST)
            }
            label {
                r#for : "unnest_ast",
                "unnest ast"
            }
            input {
                r#type : "radio",
                id : "init_cfg",
                name : "comp_view",
                value : "init_cfg",
                onclick : move |_| comp_view.set(ViewMode::InitCFG)
            }
            label {
                r#for : "init_cfg",
                "initial cfg"
            }
            input {
                r#type : "radio",
                id : "clean_cfg",
                name : "comp_view",
                value : "clean_cfg",
                onclick : move |_| comp_view.set(ViewMode::CleanCFG)
            }
            label {
                r#for : "clean_cfg",
                "clean cfg"
            }
            input {
                r#type : "radio",
                id : "cfg_annot",
                name : "comp_view",
                value : "cfg_annot",
                onclick : move |_| comp_view.set(ViewMode::CFGAnnot)
            }
            label {
                r#for : "cfg_annot",
                "lva"
            }
            input {
                r#type : "radio",
                id : "rem_write",
                name : "comp_view",
                value : "rem_write",
                onclick : move |_| comp_view.set(ViewMode::UnWrite)
            }
            label {
                r#for : "rem_write",
                "unused writes"
            }
        }
        rsx! {FrontEndView { input_program: cx.props.input_program, goal: *comp_view.get() }}

    })
}

// create a component that renders a div with the text "Hello, world!"
fn App(cx: Scope) -> Element {
    let code = use_state(cx, || include_str!("sample.expl").to_string());
    cx.render(rsx! {
        body {
            style : "background-color:#515151;",
            div {
                style : "background-color:#aaaaaa; width: 100%; min-height: 100%;",
                p {
                    b {"Hello"} a {", world! :smile: "}
                }
            }
            div {
                style: r"min-height: 60%;",
                div {
                    style: r"float:left; width: 50%; min-height: 90%; font-family: monospace; background-color:#515151;",
                    textarea {
                        style: r"width: 90%; min-height: 90%; font-family: monospace; cols: 20",
                        // we tell the component what to render
                        value: "{code}",
                        // and what to do when the value changes
                        oninput: move |evt| code.set(evt.value.clone()),
                    }
                }
                div {
                    style: r"float:right; width: 50%; min-height: 90%; font-family: monospace; background-color:#515151; color:white",
                    CompilerSelect { input_program: &code.get()}
                }
            }
            /*div {
                style: r"clear: both; font-family: monospace;",
                "{error_msg}"
            }*/
        }
    })
}

fn main() {
    // launch the web app
    dioxus_web::launch(App);
}
