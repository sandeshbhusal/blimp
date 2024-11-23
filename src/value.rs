use std::ops::{Add, Div, Mul, Rem, Sub};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I32(i32),
    F32(f32),
    Bool(bool),
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::I32(value)
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Value::F32(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Value::I32(lhs), Value::I32(rhs)) => Value::I32(lhs + rhs),
            (Value::F32(lhs), Value::F32(rhs)) => Value::F32(lhs + rhs),
            (Value::I32(lhs), Value::F32(rhs)) => Value::F32(lhs as f32 + rhs),
            (Value::F32(lhs), Value::I32(rhs)) => Value::F32(lhs + rhs as f32),
            _ => panic!("Type mismatch in addition operation"),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (Value::I32(lhs), Value::I32(rhs)) => Value::I32(lhs - rhs),
            (Value::F32(lhs), Value::F32(rhs)) => Value::F32(lhs - rhs),
            (Value::I32(lhs), Value::F32(rhs)) => Value::F32(lhs as f32 - rhs),
            (Value::F32(lhs), Value::I32(rhs)) => Value::F32(lhs - rhs as f32),
            _ => panic!("Type mismatch in subtraction operation"),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Value::I32(lhs), Value::I32(rhs)) => Value::I32(lhs * rhs),
            (Value::F32(lhs), Value::F32(rhs)) => Value::F32(lhs * rhs),
            (Value::I32(lhs), Value::F32(rhs)) => Value::F32(lhs as f32 * rhs),
            (Value::F32(lhs), Value::I32(rhs)) => Value::F32(lhs * rhs as f32),
            _ => panic!("Type mismatch in multiplication operation"),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (Value::I32(lhs), Value::I32(rhs)) => Value::I32(lhs / rhs),
            (Value::F32(lhs), Value::F32(rhs)) => Value::F32(lhs / rhs),
            (Value::I32(lhs), Value::F32(rhs)) => Value::F32(lhs as f32 / rhs),
            (Value::F32(lhs), Value::I32(rhs)) => Value::F32(lhs / rhs as f32),
            _ => panic!("Type mismatch in division operation"),
        }
    }
}

impl Rem for Value {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match (self, other) {
            (Value::I32(lhs), Value::I32(rhs)) => Value::I32(lhs % rhs),
            (Value::F32(lhs), Value::F32(rhs)) => Value::F32(lhs % rhs),
            (Value::I32(lhs), Value::F32(rhs)) => Value::F32(lhs as f32 % rhs),
            (Value::F32(lhs), Value::I32(rhs)) => Value::F32(lhs % rhs as f32),
            _ => panic!("Type mismatch in modulo operation"),
        }
    }
}


impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::I32(lhs), Value::I32(rhs)) => lhs.partial_cmp(rhs),
            (Value::F32(lhs), Value::F32(rhs)) => lhs.partial_cmp(rhs),
            (Value::I32(lhs), Value::F32(rhs)) => (*lhs as f32).partial_cmp(rhs),
            (Value::F32(lhs), Value::I32(rhs)) => lhs.partial_cmp(&(*rhs as f32)),
            _ => panic!("Type mismatch in comparison operation"),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(val) => *val,
            _ => panic!("Expected boolean value"),
        }
    }
}
