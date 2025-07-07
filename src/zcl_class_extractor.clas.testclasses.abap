" @testing ZCL_CLASS_EXTRACTOR
CLASS zcl_class_extractor_test DEFINITION
  FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      c_class_a TYPE string VALUE 'ZCL_A',
      c_class_b TYPE string VALUE 'ZCL_B',
      c_class_d TYPE string VALUE 'ZCL_D',
      c_iface_c TYPE string VALUE 'ZIF_C'.

    METHODS:
      test_class_a FOR TESTING,
      test_class_b FOR TESTING,
      test_interface_c FOR TESTING,
      test_class_d FOR TESTING,
      test_generate_mermaid FOR TESTING.

ENDCLASS.

CLASS zcl_class_extractor_test IMPLEMENTATION.

  METHOD test_class_a.
    DATA(lo_extractor) = NEW zcl_class_extractor( ).
    DATA(ls_meta) = lo_extractor->get_class_info( c_class_a ).

    " Class name
    cl_abap_unit_assert=>assert_equals(
      act = ls_meta-class_name
      exp = c_class_a ).

    " Is abstract
    cl_abap_unit_assert=>assert_true( act = ls_meta-is_abstract ).

    " Not interface
    cl_abap_unit_assert=>assert_false( act = ls_meta-is_interface ).

    " Has public attribute
    DATA(lv_attr_found) = abap_false.
    LOOP AT ls_meta-attributes INTO DATA(ls_attr).
      IF ls_attr-attr_name = 'GV_ATTR_PUBLIC' AND ls_attr-visibility = 'PUBLIC'.
        lv_attr_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( lv_attr_found ).

    " Has public method
    DATA(lv_method_found) = abap_false.
    LOOP AT ls_meta-methods INTO DATA(ls_method).
      IF ls_method-method_name = 'IF_METHOD_PUBLIC' AND ls_method-visibility = 'PUBLIC'.
        lv_method_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( lv_method_found ).
  ENDMETHOD.

  METHOD test_class_b.
    DATA(lo_extractor) = NEW zcl_class_extractor( ).
    DATA(ls_meta) = lo_extractor->get_class_info( c_class_b ).

    " Class name
    cl_abap_unit_assert=>assert_equals(
      act = ls_meta-class_name
      exp = c_class_b ).

    " Superclass should be ZCL_A
    cl_abap_unit_assert=>assert_equals(
      act = ls_meta-superclass
      exp = c_class_a ).

    " Check method B_PUBLIC_METHOD
    DATA(lv_found) = abap_false.
    LOOP AT ls_meta-methods INTO DATA(ls_method).
      IF ls_method-method_name = 'B_PUBLIC_METHOD' AND ls_method-visibility = 'PUBLIC'.
        lv_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( lv_found ).
  ENDMETHOD.

  METHOD test_interface_c.
    DATA(lo_extractor) = NEW zcl_class_extractor( ).
    DATA(ls_meta) = lo_extractor->get_class_info( c_iface_c ).

    " Class name
    cl_abap_unit_assert=>assert_equals(
      act = ls_meta-class_name
      exp = c_iface_c ).

    " Should be an interface
    cl_abap_unit_assert=>assert_true( act = ls_meta-is_interface ).

    " Should contain constant GC_CONST_PUBLIC
    DATA(lv_const_found) = abap_false.
    LOOP AT ls_meta-attributes INTO DATA(ls_attr).
      IF ls_attr-attr_name = 'GC_CONST_PUBLIC' AND ls_attr-is_constant = abap_true.
        lv_const_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( lv_const_found ).
  ENDMETHOD.

  METHOD test_class_d.
    DATA(lo_extractor) = NEW zcl_class_extractor( ).
    DATA(ls_meta) = lo_extractor->get_class_info( c_class_d ).

    " Class name
    cl_abap_unit_assert=>assert_equals(
      act = ls_meta-class_name
      exp = c_class_d ).

    " Should not be an interface
    cl_abap_unit_assert=>assert_false( act = ls_meta-is_interface ).

    " Should implement ZIF_C
    DATA(lv_iface_found) = abap_false.
    LOOP AT ls_meta-interfaces INTO DATA(lv_iface).
      IF lv_iface = c_iface_c.
        lv_iface_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( lv_iface_found ).
  ENDMETHOD.

  METHOD test_generate_mermaid.
    DATA(lo_extractor) = NEW zcl_class_extractor( ).
    DATA(lv_result) = lo_extractor->zif_extractor~generate_mermaid_code( c_class_b ).

    cl_abap_unit_assert=>assert_true( act = lv_result-success ).
    cl_abap_unit_assert=>assert_not_initial( act = lv_result-mermaid_code ).

    " Check if ZCL_B is in Mermaid code
    FIND c_class_b IN lv_result-mermaid_code MATCH COUNT DATA(lv_count).
    cl_abap_unit_assert=>assert_true( act = COND abap_bool( WHEN lv_count > 0 THEN abap_true ELSE abap_false ) ).

    " Check superclass relationship syntax
    FIND '--|>' IN lv_result-mermaid_code MATCH COUNT lv_count.
    cl_abap_unit_assert=>assert_true( act = COND abap_bool( WHEN lv_count > 0 THEN abap_true ELSE abap_false ) ).
  ENDMETHOD.

ENDCLASS.
