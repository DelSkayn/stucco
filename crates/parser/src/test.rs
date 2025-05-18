use crate::{parse_wrapped_module, Parser};

#[test]
fn empty_module() {
    let m = r#"
        mod foo {
        }
    "#;

    let (module, ast) = Parser::parse_str_func(m, parse_wrapped_module).unwrap();

    let module = &ast[module];
    assert_eq!(ast[module.sym.unwrap()].name.index(&ast), "foo");
    assert!(module.functions.is_none());
}

#[test]
fn empty_function() {
    let m = r#"
        mod foo {

            fn bar(){
            }
        }
    "#;

    let (module, ast) = Parser::parse_str_func(m, parse_wrapped_module).unwrap();

    let module = &ast[module];
    assert_eq!(module.sym.unwrap().index(&ast).name.index(&ast), "foo");
    let func = module.functions.unwrap();
    let func = ast[func].value;
    assert_eq!(func.index(&ast).sym.index(&ast).name.index(&ast), "bar");
}
