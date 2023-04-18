#![allow(non_snake_case)]
use dioxus::prelude::*;
use sample_compiler_lib::{
    source_ast::{self, id_to_string, Exp, Func, Prog, Stmt, Typ, TypedExp, VarDec},
    tokens,
};

#[derive(Props)]
pub struct ProgramCtx<'a> {
    inp: &'a Prog,
}
#[derive(Props)]
struct VarDecCtx<'a> {
    var_dec: &'a VarDec,
    indent: usize,
}
#[derive(Props)]
struct FuncCtx<'a> {
    func: &'a Func,
}
#[derive(Props)]
pub struct StmtCtx<'a> {
    stmt: &'a Stmt,
    indent: usize,
}
#[derive(Props)]
struct ExprCtx<'a> {
    exp: &'a TypedExp,
}

fn type_to_col(typ: &Option<Typ>) -> String {
    match typ {
        Some(Typ::Int) => "green",
        Some(Typ::Bool) => "orange",
        Some(Typ::Array(_)) => "blue",
        _ => "gray",
    }
    .to_string()
}

fn pad_gen(indent: usize) -> String {
    let mut pad = "".to_string();
    for _ in 0..indent {
        pad += "  ";
    }
    pad
}

fn RenderVar<'a>(cx: Scope<'a, VarDecCtx<'a>>) -> Element {
    let var_name = source_ast::id_to_string(&cx.props.var_dec.var_name);
    let var_typ = source_ast::typ_to_string(&cx.props.var_dec.typ);
    let pad = pad_gen(cx.props.indent);
    let col = type_to_col(&Some(cx.props.var_dec.typ));

    cx.render(rsx! {
        div { "{pad}" span { style : "background-color:{col}", "{var_name} {var_typ};" }}
    })
}

fn RenderExp<'a>(cx: Scope<'a, ExprCtx<'a>>) -> Element {
    let col = type_to_col(&cx.props.exp.typ);
    let elm = match &cx.props.exp.exp {
        Exp::Ident(id, es) => {
            let var_name = source_ast::id_to_string(id);
            let idxs = es.iter().map(|e| rsx! {"[" RenderExp{exp : e} "]"});
            rsx! {
                "({var_name}" idxs ")"
            }
        }
        Exp::Call(id, es) => {
            let func_name = source_ast::id_to_string(id);
            let idxs = es.iter().map(|e| rsx! { RenderExp{exp : e} ", "});
            rsx! {
                "({func_name}" "(" idxs "))"
            }
        }
        Exp::Num(n) => {
            rsx! {
                "({n})"
            }
        }
        Exp::Bool(b) => {
            rsx! {
                "({b})"
            }
        }
        Exp::Op(e0, op, e1) => {
            rsx! {
                "(" RenderExp {exp : e0} tokens::show_op(op) RenderExp{exp : e1} ")"
            }
        }
        Exp::Uop(uop, e0) => {
            rsx! {
                "(" tokens::show_uop(uop) RenderExp {exp : &*e0} ")"
            }
        }
        Exp::Array(es) => {
            let idxs = es.iter().map(|e| rsx! {"[" RenderExp{exp : e} "]"});
            rsx! {
                "array" idxs
            }
        }
    };
    //let col = type_to_col(type_check);
    cx.render(rsx! {
        span {
            style : "background-color:{col}",
            elm
        }
    })
}

pub fn RenderStmt<'a>(cx: Scope<'a, StmtCtx<'a>>) -> Element {
    let pad = pad_gen(cx.props.indent);
    match cx.props.stmt {
        Stmt::Assign(id, inds, e) => {
            let var_name = source_ast::id_to_string(id);
            let idxs = inds.iter().map(|e| rsx! {"[" RenderExp{exp : e} "]"});
            cx.render(rsx! {
                "{pad}{var_name}" idxs " = " RenderExp{exp: e} "\n"
            })
        }
        Stmt::DoWhile(s0, e, s1) => cx.render(rsx! {
            RenderStmt {stmt : s0, indent: cx.props.indent}
            "{pad}while (" RenderExp{exp: e} ") \n"
            RenderStmt {stmt : s1, indent: cx.props.indent}
        }),
        Stmt::Ite(e, s0, s1) => cx.render(rsx! {
            "{pad}if (" RenderExp{exp: e} ") then\n"
            RenderStmt {stmt : s0, indent: cx.props.indent}
            "{pad}else\n"
            RenderStmt {stmt : s1, indent: cx.props.indent}
        }),
        Stmt::Stmts(stmts) => {
            let stmts = stmts
                .iter()
                .map(|s| rsx! {RenderStmt {stmt : s, indent: cx.props.indent + 1}});
            cx.render(rsx! {
                "{pad}{{\n",
                stmts,
                "{pad}}}\n"
            })
        }
        Stmt::In(id) => cx.render(rsx! {
            "{pad}input " id_to_string(id) "\n"
        }),
        Stmt::Out(id) => cx.render(rsx! {
            "{pad}output " id_to_string(id) "\n"
        }),
        Stmt::Return(Some(id)) => cx.render(rsx! {
            "{pad}return " id_to_string(id) "\n"
        }),
        Stmt::Return(None) => cx.render(rsx! {
            "{pad}return\n"
        }),
        Stmt::Loc(stmt, _) => cx.render(rsx! {
            RenderStmt {stmt : stmt, indent: cx.props.indent}
        }),
    }
}

fn RenderFunc<'a>(cx: Scope<'a, FuncCtx<'a>>) -> Element {
    let fun_name = source_ast::id_to_string(&cx.props.func.fun_name);
    let fun_typ = source_ast::typ_to_string(&cx.props.func.ret);
    let params: String = cx
        .props
        .func
        .params
        .iter()
        .map(|(id, t)| source_ast::id_to_string(id) + " " + &source_ast::typ_to_string(t) + ", ")
        .collect();
    let locals = cx
        .props
        .func
        .locals
        .iter()
        .map(|local| rsx! {RenderVar {var_dec : &local, indent : 1}});
    let body = cx
        .props
        .func
        .body
        .iter()
        .map(|stmt| rsx! {RenderStmt {stmt : &stmt, indent : 1}});
    cx.render(rsx! {
        pre { "{fun_name} ({params}) -> {fun_typ}" }
        div {
            pre {
                locals
            }
        }
        div {
            pre {
                body
            }
        }
    })
}

pub fn RenderProg<'a>(cx: Scope<'a, ProgramCtx<'a>>) -> Element {
    let vars = cx
        .props
        .inp
        .globals
        .iter()
        .enumerate()
        .map(|(idx, v)| rsx! {RenderVar{var_dec : &v, indent : 0} });
    let funcs = cx
        .props
        .inp
        .funcs
        .iter()
        .enumerate()
        .map(|(idx, f)| rsx! {div{ key: "{idx}", RenderFunc {func : &f}}});
    cx.render(rsx! {
        div {
            vars
        }
        div {
            funcs
        }
    })
}
