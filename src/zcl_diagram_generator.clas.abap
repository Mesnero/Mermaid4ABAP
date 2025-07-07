CLASS zcl_diagram_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_generation_result,
             object_name TYPE string,
             object_type TYPE string,
             mermaid_code TYPE string,
             success TYPE abap_bool,
             message TYPE string,
           END OF ty_generation_result.

    METHODS: generate_diagram
               IMPORTING iv_object_name TYPE string
                        iv_object_type TYPE string OPTIONAL
               RETURNING VALUE(rs_result) TYPE ty_generation_result.

  PRIVATE SECTION.
    DATA mo_extractor TYPE REF TO zif_extractor.

    METHODS: determine_object_type
               IMPORTING iv_object_name TYPE string
               RETURNING VALUE(rv_type) TYPE string,

             validate_object_exists
               IMPORTING iv_object_name TYPE string
                        iv_object_type TYPE string
               RETURNING VALUE(rv_exists) TYPE abap_bool.
ENDCLASS.

CLASS zcl_diagram_generator IMPLEMENTATION.

  METHOD generate_diagram.

    rs_result-object_name = iv_object_name.
    rs_result-object_type = iv_object_type.

    " Auto-detect object type if not provided
    IF iv_object_type IS INITIAL.
      rs_result-object_type = determine_object_type( iv_object_name ).
    ENDIF.

    " Validate object exists
    IF validate_object_exists( iv_object_name = iv_object_name
                              iv_object_type = rs_result-object_type ) = abap_false.
      rs_result-success = abap_false.
      rs_result-message = |Object { iv_object_name } not found or not accessible|.
      RETURN.
    ENDIF.

    TRY.
        mo_extractor ?= zcl_extractor_factory=>create_extractor( iv_type = iv_object_type ).
    CATCH cx_root INTO DATA(lx_filetype).
        rs_result-success = abap_false.
        rs_result-message = |Filetype { lx_filetype->get_text(  ) } not supported.|.
        RETURN.
    ENDTRY.

    TRY.
        DATA(ls_cds_structure) = mo_extractor->generate_mermaid_code( iv_object_name ).
        rs_result-success = ls_cds_structure-success.
        rs_result-message = ls_cds_structure-message.
        rs_result-mermaid_code = ls_cds_structure-mermaid_code.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-message = |Error generating diagram: { lx_error->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD determine_object_type.
    " Enhanced object type detection including behavior definitions
    IF iv_object_name CS 'ZC_' OR iv_object_name CS 'ZI_' OR
       iv_object_name CS 'YC_' OR iv_object_name CS 'YI_' OR
       iv_object_name CS '_CDS' OR iv_object_name CP '*CDS*'.
      rv_type = 'CDS'.
    ELSEIF iv_object_name CS '_VIEW' OR iv_object_name CP 'Z*_V' OR iv_object_name CP 'Y*_V'.
      rv_type = 'VIEW'.
    ELSEIF iv_object_name CS '_BDEF' OR iv_object_name CP 'Z*BEHAVIOR*' OR
           iv_object_name CP 'Y*BEHAVIOR*' OR iv_object_name CP '*_BDEF'.
      rv_type = 'BDEF'.
    ELSEIF iv_object_name CP 'ZCL_*' OR iv_object_name CP 'YCL_*' OR
           iv_object_name CP 'ZIF_*' OR iv_object_name CP 'YIF_*'.
      rv_type = 'CLASS'.
    ELSE.
      " Try CDS first as default
      rv_type = 'CDS'.
    ENDIF.
  ENDMETHOD.

  METHOD validate_object_exists.
    " Simple validation using RTTI - if we can describe it, it exists
    TRY.
        DATA(lo_descr) = cl_abap_typedescr=>describe_by_name( iv_object_name ).
        rv_exists = abap_true.
      CATCH cx_root.
        rv_exists = abap_false.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
