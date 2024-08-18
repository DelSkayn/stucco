use crate::Parser;

#[test]
fn empty_module() {
    let m = r#"
        mod foo {
        }
    "#;

    let (module, ast) = Parser::parse_str::<ast::Module>(m).unwrap();

    let module = &ast[module];
    assert_eq!(ast[module.name], "foo");
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

    let (module, ast) = Parser::parse_str::<ast::Module>(m).unwrap();

    let module = &ast[module];
    assert_eq!(ast[module.name], "foo");
    let func = module.functions.unwrap();
    let func = ast[func].value;
    assert_eq!(func.index(&ast).name.index(&ast), "bar");
}
