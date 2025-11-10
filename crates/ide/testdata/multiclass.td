class Field;

multiclass Multiclass1 {
    def _field1: Field {
      Field sub_field;
    }
    def _field2: Field;
    def : Field;
    def "": Field;
}

multiclass Multiclass2 {
    defm _field1 : Multiclass1;
    defm _field2 : Multiclass1;
}

defm multiclass1 : Multiclass1;
defm multiclass2 : Multiclass1;

defm multiclass2_2 : Multiclass2;

defvar var = multiclass1_field1.sub_field;