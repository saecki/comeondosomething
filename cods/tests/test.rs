use cods::{calc, PlainVal};

#[test]
fn test1() {
    assert_eq!(
        PlainVal::Float(20713257.3385426),
        calc("234.4234 + 6345.423 * 3264.2462").0.unwrap(),
    );
}

#[test]
fn test2() {
    assert_eq!(
        PlainVal::Int(-9717750),
        calc("6 + 3452 − (3252 × 5324) + (((2342 × 3242) ÷ 4234) × 4234) − 324").0.unwrap(),
    );
}

#[test]
fn test3() {
    assert_eq!(
        PlainVal::Float(41968480425.587155963),
        calc("(23423 × 423 + (423 − 234) ÷ 654 + 4324) × 4234").0.unwrap(),
    );
}
