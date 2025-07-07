CLASS zcl_extractor_utilities DEFINITION
  ABSTRACT
  PUBLIC
  CREATE PUBLIC .


  PUBLIC SECTION.

    TYPES: BEGIN OF ty_error_info,
             error_code    TYPE string,
             error_message TYPE string,
             error_context TYPE string,
             timestamp     TYPE timestampl,
           END OF ty_error_info.

    DATA: mv_last_error_code    TYPE string,
          mv_last_error_message TYPE string,
          mv_last_error_context TYPE string.

    METHODS:
      " Get last error information
      get_last_error
        RETURNING VALUE(rs_error) TYPE ty_error_info.

  PROTECTED SECTION.

    METHODS:
      " Get system information for error context
      get_system_context
        RETURNING VALUE(rv_context) TYPE string.
ENDCLASS.



CLASS zcl_extractor_utilities IMPLEMENTATION.

  METHOD get_last_error.
    rs_error-error_code = mv_last_error_code.
    rs_error-error_message = mv_last_error_message.
    rs_error-error_context = mv_last_error_context.
    rs_error-timestamp = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
  ENDMETHOD.

  METHOD get_system_context.
    TRY.
        rv_context = |System: { sy-sysid }, Client: { sy-mandt }, User: { sy-uname }|.
      CATCH cx_root.
        rv_context = 'System context unavailable'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
