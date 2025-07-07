" @testing ZCL_CDS_EXTRACTOR
CLASS zcl_cds_extractor_test DEFINITION
  FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS c_view TYPE string VALUE 'ZCDS_EMPLOYEES'.

    METHODS:
      test_extract FOR TESTING,
      test_mermaid FOR TESTING.
ENDCLASS.

CLASS zcl_cds_extractor_test IMPLEMENTATION.

  METHOD test_extract.
    " Create extractor instance
    DATA(lo_extractor) = NEW zcl_cds_extractor( ).
    DATA(ls_meta) = lo_extractor->extract_cds_metadata( c_view ).

    " ===== ACTUAL TEST ASSERTIONS =====
    " Assert correct CDS view name
    cl_abap_unit_assert=>assert_equals(
      act = ls_meta-view_name
      exp = c_view ).

    " Assert 3 fields returned
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_meta-fields )
      exp = 3 ).

    " Field 1: EMP_ID (should be key)
    DATA(ls_field) = ls_meta-fields[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = ls_field-field_name exp = 'EMP_ID' ).
    cl_abap_unit_assert=>assert_true( act = ls_field-key_field ).

    " Field 2: NAME
    ls_field = ls_meta-fields[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = ls_field-field_name exp = 'NAME' ).
    cl_abap_unit_assert=>assert_false( act = ls_field-key_field ).

    " Field 3: DEPARTMENT
    ls_field = ls_meta-fields[ 3 ].
    cl_abap_unit_assert=>assert_equals( act = ls_field-field_name exp = 'DEPARTMENT' ).
    cl_abap_unit_assert=>assert_false( act = ls_field-key_field ).
  ENDMETHOD.

  METHOD test_mermaid.
    DATA(lo_extractor) = NEW zcl_cds_extractor( ).
    " Generate Mermaid diagram
    DATA(lv_mermaid) = lo_extractor->zif_extractor~generate_mermaid_code( c_view ).
    cl_abap_unit_assert=>assert_not_initial( lv_mermaid ).
    DATA(lv_mermaid_code) = lv_mermaid-mermaid_code.

    " ===== ACTUAL TEST ASSERTIONS =====

    " Check for key field in Mermaid output
    FIND 'EMP_ID PK' IN lv_mermaid_code MATCH COUNT DATA(lv_count).
    cl_abap_unit_assert=>assert_true(
      act = COND abap_bool( WHEN lv_count > 0 THEN abap_true ELSE abap_false ) ).

    FIND 'NAME' IN lv_mermaid_code MATCH COUNT lv_count.
    cl_abap_unit_assert=>assert_true(
      act = COND abap_bool( WHEN lv_count > 0 THEN abap_true ELSE abap_false ) ).

    FIND 'DEPARTMENT' IN lv_mermaid_code MATCH COUNT lv_count.
    cl_abap_unit_assert=>assert_true(
      act = COND abap_bool( WHEN lv_count > 0 THEN abap_true ELSE abap_false ) ).
  ENDMETHOD.

ENDCLASS.
