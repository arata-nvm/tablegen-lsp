class Field;

multiclass Multiclass1 {
    def _field1: Field;
    def _field2: Field;
}

multiclass Multiclass2 {
    defm _field1 : Multiclass1;
}

defm multiclass1 : Multiclass1;
defm multiclass2 : Multiclass1;

defm multiclass2_2 : Multiclass2;
