use tablegen_parser::{ast, node::SyntaxNode};

use crate::{
    evaluator::Evaluator,
    record_keeper::{Record, RecordField, RecordKeeper, TemplateArg, Type, Value},
};

pub fn evaluate(root: SyntaxNode) -> Option<RecordKeeper> {
    let mut e = Evaluator::new();
    eval_file(root, &mut e);
    Some(e.finish())
}

fn eval_file(root: SyntaxNode, e: &mut Evaluator) -> Option<()> {
    let file = root.cast::<ast::File>()?;
    let statement_list = file.statement_list()?;
    for statement in statement_list.statements() {
        eval_statement(statement, e);
    }
    None
}

fn eval_statement(statement: ast::Statement, e: &mut Evaluator) -> Option<()> {
    match statement {
        ast::Statement::Include(_) => todo!(),
        ast::Statement::Class(class) => eval_class(class, e),
        ast::Statement::Def(_) => todo!(),
        ast::Statement::Let(_) => todo!(),
    };
    None
}

fn eval_class(class: ast::Class, e: &mut Evaluator) -> Option<()> {
    let name = class.name()?.value()?;
    e.start_record(Record::new(name.clone()));

    for arg in class.template_arg_list()?.args() {
        let arg = eval_template_arg(arg, e)?;
        e.add_record_template_arg(arg);
    }
    eval_record_body(class.record_body()?, e);

    e.finish_record();
    None
}

fn eval_template_arg(arg: ast::TemplateArgDecl, e: &mut Evaluator) -> Option<TemplateArg> {
    let name = arg.name()?.value()?;
    let typ = eval_type(arg.r#type()?, e)?;
    let value = eval_value(arg.value()?, e)?;
    Some(TemplateArg::new(name.clone(), typ, value))
}

fn eval_record_body(record_body: ast::RecordBody, e: &mut Evaluator) -> Option<()> {
    for parent_class in record_body.parent_class_list()?.classes() {}
    for body_item in record_body.body()?.items() {
        eval_body_item(body_item, e);
    }
    None
}

fn eval_body_item(body_item: ast::BodyItem, e: &mut Evaluator) -> Option<()> {
    match body_item {
        ast::BodyItem::FieldDef(def) => {
            let name = def.name()?.value()?;
            let typ = eval_type(def.r#type()?, e)?;
            let value = match def.value() {
                Some(value) => eval_value(value, e)?,
                None => Value::Uninitialized,
            };
            let field = RecordField::new(name.clone(), typ, value);
            e.add_record_field(field);
        }
        ast::BodyItem::FieldLet(_) => todo!(),
        ast::BodyItem::Defvar(_) => todo!(),
        ast::BodyItem::Assert(_) => todo!(),
    }
    None
}

fn eval_type(typ: ast::Type, e: &mut Evaluator) -> Option<Type> {
    let typ = match typ {
        ast::Type::BitType(_) => Type::Bit,
        ast::Type::IntType(_) => Type::Int,
        ast::Type::StringType(_) => Type::String,
        ast::Type::DagType(_) => Type::Dag,
        ast::Type::BitsType(t) => Type::Bits(t.length()?.value()?),
        ast::Type::ListType(t) => Type::List(Box::new(eval_type(t.inner_type()?, e)?)),
        ast::Type::ClassId(t) => Type::Class(t.name()?.value()?.to_string()),
        ast::Type::CodeType(_) => Type::Code,
    };
    Some(typ)
}

fn eval_value(value: ast::Value, e: &mut Evaluator) -> Option<Value> {
    for inner_value in value.inner_values() {
        let value = eval_simple_value(inner_value.simple_value()?, e)?;
        for suffix in inner_value.suffixes() {
            match suffix {
                ast::ValueSuffix::RangeSuffix(_) => todo!(),
                ast::ValueSuffix::SliceSuffix(_) => todo!(),
                ast::ValueSuffix::FieldSuffix(_) => todo!(),
            }
        }
        return Some(value);
    }
    None
}

fn eval_simple_value(simple_value: ast::SimpleValue, e: &mut Evaluator) -> Option<Value> {
    let val = match simple_value {
        ast::SimpleValue::Integer(v) => Value::Integer(v.value()?),
        ast::SimpleValue::String(v) => Value::String(v.value()),
        ast::SimpleValue::Code(v) => Value::Code(v.value()?.to_string()),
        ast::SimpleValue::Boolean(v) => Value::Boolean(v.value()?),
        ast::SimpleValue::Uninitialized(_) => Value::Uninitialized,
        ast::SimpleValue::Bits(v) => Value::Bits(
            v.value_list()?
                .values()
                .map(|v| eval_value(v, e))
                .collect::<Option<Vec<Value>>>()?,
        ),
        ast::SimpleValue::List(v) => Value::List(
            v.value_list()?
                .values()
                .map(|v| eval_value(v, e))
                .collect::<Option<Vec<Value>>>()?,
        ),
        ast::SimpleValue::Dag(v) => todo!(),
        ast::SimpleValue::Identifier(v) => Value::Identifier(v.value()?.to_string()),
        ast::SimpleValue::ClassRef(v) => {
            let name = v.name()?.value()?.to_string();
            let positional_arg = v
                .arg_value_list()?
                .positional()?
                .values()
                .map(|v| eval_value(v, e))
                .collect::<Option<Vec<Value>>>()?;
            let named_arg = v
                .arg_value_list()?
                .named()?
                .values()
                .map(|v| Option::zip(eval_value(v.name()?, e), eval_value(v.value()?, e)))
                .collect::<Option<Vec<(Value, Value)>>>()?;
            Value::ClassRef(name, positional_arg, named_arg)
        }
        ast::SimpleValue::BangOperator(v) => {
            let op = v.kind()?;
            let args = v
                .values()
                .map(|v| eval_value(v, e))
                .collect::<Option<Vec<Value>>>()?;
            Value::BangOperator(op, args)
        }
        ast::SimpleValue::CondOperator(v) => {
            let clauses = v
                .clauses()
                .map(|c| Option::zip(eval_value(c.condition()?, e), eval_value(c.value()?, e)))
                .collect::<Option<Vec<(Value, Value)>>>()?;
            Value::CondOperator(clauses)
        }
    };
    Some(val)
}
