#![allow(non_snake_case)]
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use dioxus::prelude::*;

use sample_compiler_lib::block_structure::var_to_string;
use sample_compiler_lib::live_var_analysis::CFGAnnot;
use sample_compiler_lib::{
    block_structure::{
        self, atomic_to_string, test_to_string, AtomicExp, BlockElem, CFGEntry, NextBlock,
    },
    tokens::show_op,
};

#[derive(Props)]
pub struct ProgramCFG<'a> {
    cfg: &'a Vec<CFGEntry>,
}

#[derive(Props)]
pub struct ProgramCFGEnt<'a> {
    ent: &'a CFGEntry,
}

#[derive(Props)]
pub struct ProgramAnnotCFG<'a> {
    cfg: &'a Vec<(CFGEntry, CFGAnnot)>,
}

#[derive(Props)]
pub struct ProgramAnnotCFGEnt<'a> {
    ent: &'a CFGEntry,
    annot: &'a CFGAnnot,
}

#[derive(Props)]
pub struct ProgramAnnot<'a> {
    annot: &'a CFGAnnot,
}

#[derive(Props)]
pub struct ProgramBlockElem<'a> {
    elm: &'a BlockElem,
}

//https://stackoverflow.com/questions/29573605/how-do-i-use-stdhashhash Shepmasters answer
fn my_hash<T>(obj: T) -> u64
where
    T: Hash,
{
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

fn string_to_col(s: &str) -> String {
    let hash = my_hash(s) & 0x7f7f7f + 0x808080;
    format!("#{:06x}", hash)
}

pub fn RenderBlockElem<'a>(cx: Scope<'a, ProgramBlockElem<'a>>) -> Element {
    let elm = cx.props.elm;
    let e = match elm {
        BlockElem::AssignOp(v, exp0, op, exp1) => {
            let var_name = block_structure::var_to_string(v);
            let var_col = string_to_col(&var_name);
            let exp0_name = atomic_to_string(exp0);
            let exp0_col = string_to_col(&exp0_name);
            let exp1_name = atomic_to_string(exp1);
            let exp1_col = string_to_col(&exp1_name);
            rsx! {
                span {
                    style: "color: {var_col}",
                    "{var_name}"
                }
                " = "
                span {
                    style: "color: {exp0_col}",
                    "{exp0_name}"
                }
                " " show_op(op) " "
                span {
                    style: "color: {exp1_col}",
                    "{exp1_name}"
                }
            }
        }
        BlockElem::AssignAtom(v, exp) => {
            let var_name = block_structure::var_to_string(v);
            let var_col = string_to_col(&var_name);
            let exp_name = atomic_to_string(exp);
            let exp_col = string_to_col(&exp_name);
            rsx! {
                span {
                    style: "color: {var_col}",
                    "{var_name}"
                }
                " = "
                span {
                    style: "color: {exp_col}",
                    "{exp_name}"
                }
            }
        }
        BlockElem::Ld(v0, v1, exp) => {
            let var_name0 = block_structure::var_to_string(v0);
            let var_col0 = string_to_col(&var_name0);
            let var_name1 = block_structure::var_to_string(v1);
            let var_col1 = string_to_col(&var_name1);
            let exp_name = atomic_to_string(exp);
            let exp_col = string_to_col(&exp_name);
            rsx! {
                span {
                    style: "color: {var_col0}",
                    "{var_name0}"
                }
                " = "
                span {
                    style: "color: {var_col1}",
                    "{var_name1}"
                }
                "["
                span {
                    style: "color: {exp_col}",
                    "{exp_name}"
                }
                "]"
            }
        }
        BlockElem::St(v, exp0, exp1) => {
            let var_name = block_structure::var_to_string(v);
            let var_col = string_to_col(&var_name);
            let exp_name0 = atomic_to_string(exp0);
            let exp_col0 = string_to_col(&exp_name0);
            let exp_name1 = atomic_to_string(exp1);
            let exp_col1 = string_to_col(&exp_name1);
            rsx! {
                span {
                    style: "color: {var_col}",
                    "{var_name}"
                }
                "["
                span {
                    style: "color: {exp_col0}",
                    "{exp_name0}"
                }
                "] = "
                span {
                    style: "color: {exp_col1}",
                    "{exp_name1}"
                }
            }
        }
        BlockElem::Call(r, f, es) => {
            let var_name = match r {
                Some(r) => block_structure::var_to_string(r),
                None => "none".to_string(),
            };
            let var_col = string_to_col(&var_name);

            let exps = es.iter().map(|exp| {
                let exp_name = atomic_to_string(exp);
                let exp_col = string_to_col(&exp_name);
                rsx! {
                    span {
                        style: "color: {exp_col}",
                        "{exp_name}"
                    }
                    ", "
                }
            });
            rsx! {
                span {
                    style: "color: {var_col}",
                    "{var_name}"
                }
                " = {f}(" exps ")"
            }
        }
        BlockElem::BoundCheck(exp0, exp1) => {
            let exp_name0 = atomic_to_string(exp0);
            let exp_col0 = string_to_col(&exp_name0);
            let exp_name1 = atomic_to_string(exp1);
            let exp_col1 = string_to_col(&exp_name1);
            rsx! {
                "BoundCheck "
                span {
                    style: "color: {exp_col0}",
                    "{exp_name0}"
                }
                ", "
                span {
                    style: "color: {exp_col1}",
                    "{exp_name1}"
                }
            }
        }
        BlockElem::NullCheck(v) => {
            let var_name = block_structure::var_to_string(v);
            let var_col = string_to_col(&var_name);
            rsx! {
                "NullCheck "
                span {
                    style: "color: {var_col}",
                    "{var_name}"
                }
            }
        }
    };
    cx.render(rsx! {
        e
    })
}

pub fn RenderCFGEntry<'a>(cx: Scope<'a, ProgramCFGEnt<'a>>) -> Element {
    let ent = cx.props.ent;
    let elems = ent
        .elems
        .iter()
        .map(|elm| rsx! { "  " RenderBlockElem {elm: elm} "\n"});
    let next = match &ent.next {
        NextBlock::Next(x) => rsx! {"next: {x}"},
        NextBlock::Branch(t, a, b) => rsx! { "if " test_to_string(t) " then {a} else {b}"},
        NextBlock::Return(v) => rsx! { "ret" },
    };
    return cx.render(rsx! {
        div {
            style: "background-color:#515151",
            "block_num: {ent.bnum}"
        }
        div {
            style: "background-color:#313131",
            pre {
                elems
            }
        }
        div {
            style: "background-color:#515151",
            next
        }
        div {
            style: "background-color:#313131",
            pre{"\n"}
        }
    });
}

pub fn RenderAnnot<'a>(cx: Scope<'a, ProgramAnnot<'a>>) -> Element {
    let annot = cx.props.annot;
    let gen = annot.gen.iter().map(|v| {
        let var_name = var_to_string(v);
        let var_col = string_to_col(&var_name);
        rsx! {
            span {
                style: "color: {var_col}",
                "{var_name}"
            }
            ", "
        }
    });
    let kill = annot.kill.iter().map(|v| {
        let var_name = var_to_string(v);
        let var_col = string_to_col(&var_name);
        rsx! {
            span {
                style: "color: {var_col}",
                "{var_name}"
            }
            ", "
        }
    });
    let live_exit = annot.live_exit.iter().map(|v| {
        let var_name = var_to_string(v);
        let var_col = string_to_col(&var_name);
        rsx! {
            span {
                style: "color: {var_col}",
                "{var_name}"
            }
            ", "
        }
    });
    cx.render(rsx! {
        div{"gen " gen "\n"}
        div{"kill " kill "\n"}
        div{"live_exit " live_exit "\n"}
    })
}

pub fn RenderAnnotCFGEntry<'a>(cx: Scope<'a, ProgramAnnotCFGEnt<'a>>) -> Element {
    let ent = cx.props.ent;
    let annot = cx.props.annot;
    let elems = ent
        .elems
        .iter()
        .map(|elm| rsx! { "  " RenderBlockElem {elm: elm} "\n"});
    let next = match &ent.next {
        NextBlock::Return(_) => rsx! {"next: ret"},
        NextBlock::Next(x) => rsx! {"next: {x}"},
        NextBlock::Branch(t, a, b) => rsx! { "if " test_to_string(t) " then {a} else {b}"},
    };
    return cx.render(rsx! {
        div {
            style: "background-color:#515151",
            "block_num: {ent.bnum}"
        }
        div {
            style: "background-color:#313131",
            pre {
                elems
            }
        }
        div {
            style: "background-color:#515151",
            next,
            RenderAnnot {annot: annot}
        }
        div {
            style: "background-color:#313131",
            pre{"\n"}
        }
    });
}

pub fn RenderCFG<'a>(cx: Scope<'a, ProgramCFG<'a>>) -> Element {
    let cfg = cx
        .props
        .cfg
        .iter()
        .map(|ent| rsx! {div {RenderCFGEntry {ent: ent}}});
    return cx.render(rsx! {
        cfg
    });
}

pub fn RenderAnnotCFG<'a>(cx: Scope<'a, ProgramAnnotCFG<'a>>) -> Element {
    let cfg = cx
        .props
        .cfg
        .iter()
        .map(|(ent, annot)| rsx! {div {RenderAnnotCFGEntry {ent: ent, annot: annot}}});
    return cx.render(rsx! {
        cfg
    });
}
