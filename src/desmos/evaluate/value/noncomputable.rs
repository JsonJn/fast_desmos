use crate::desmos::evaluate::{
    Computable, NonCompList, NonCompPrim, PrimList, Primitive, VarValue,
};

#[derive(Debug, Clone)]
pub enum NonComputable {
    Prim(NonCompPrim),
    List(NonCompList),
}

impl From<NonComputable> for VarValue {
    fn from(value: NonComputable) -> Self {
        match value {
            NonComputable::Prim(p) => Self::Prim(Primitive::NonComputable(p)),
            NonComputable::List(l) => Self::List(PrimList::NonComputable(l)),
        }
    }
}

impl TryFrom<VarValue> for NonComputable {
    type Error = Computable;

    fn try_from(value: VarValue) -> Result<Self, Self::Error> {
        match value {
            VarValue::Prim(Primitive::NonComputable(nc)) => Ok(Self::Prim(nc)),
            VarValue::List(PrimList::NonComputable(nc)) => Ok(Self::List(nc)),
            otherwise => Err(otherwise.try_into().unwrap()),
        }
    }
}
