CLASS zcl_extractor_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS create_extractor
      IMPORTING
        iv_type    TYPE string
      RETURNING
        VALUE(ro_extractor) TYPE REF TO zif_extractor
      RAISING
        cx_abap_invalid_value.
ENDCLASS.

CLASS zcl_extractor_factory IMPLEMENTATION.
  METHOD create_extractor.
    CASE to_upper( iv_type ).
      WHEN 'CDS' OR 'VIEW'.
        ro_extractor = NEW zcl_cds_extractor( ).
      WHEN 'CLASS' OR 'INTF'.
        ro_extractor = NEW zcl_class_extractor(  ).
      WHEN 'BDEF'.
        ro_extractor = NEW zcl_behavior_extractor(  ).
      WHEN OTHERS.
       RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = iv_type. "TODO: WRONG EXCEPTION, BUT DIDN'T FIND FITTING ONE
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
