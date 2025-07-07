INTERFACE zif_extractor
  PUBLIC.
  TYPES: BEGIN OF ty_mermaid_result,
           mermaid_code TYPE string,
           success      TYPE abap_bool,
           message      TYPE string,
         END OF ty_mermaid_result.

  METHODS generate_mermaid_code
    IMPORTING
      iv_object_name   TYPE string
    RETURNING
      VALUE(rs_result) TYPE ty_mermaid_result.

ENDINTERFACE.
