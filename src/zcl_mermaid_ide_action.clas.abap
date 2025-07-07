CLASS zcl_mermaid_ide_action DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_aia_action.
  PROTECTED SECTION.
  PRIVATE SECTION.

    " Change to switch between code and diagram
    CONSTANTS use_graph TYPE abap_bool VALUE abap_true.

    METHODS:
      map_to_diagram_type
        IMPORTING iv_object_type         TYPE string
        RETURNING VALUE(rv_diagram_type) TYPE string.

ENDCLASS.


CLASS zcl_mermaid_ide_action IMPLEMENTATION.
  METHOD if_aia_action~run.
    " Get information from selected file
    DATA(focused_resource) = context->get_focused_resource(  ).
    DATA(object_name) = focused_resource->get_name(  ).
    DATA(object_type) = focused_resource->get_type(  ).

    DATA(diagram_type) = map_to_diagram_type( object_type ).

    " Use ZCL_DIAGRAM_GENERATOR to generate the Mermaid code
    DATA diagram_generator TYPE REF TO zcl_diagram_generator.
    CREATE OBJECT diagram_generator.

    DATA diagram_result TYPE zcl_diagram_generator=>ty_generation_result.

    diagram_generator->generate_diagram(
        EXPORTING
            iv_object_name = object_name
            iv_object_type = diagram_type
        RECEIVING
            rs_result = diagram_result
    ).

    " Check if there were errors during generation and display HTML for errors or the real HTML.
    DATA html TYPE string.
    IF diagram_result-success = abap_false.
      html = zcl_html_generator=>get_html_content_error(
                    iv_diagram_type = diagram_type
                    iv_object_name = object_name
                    iv_error_msg = diagram_result-message
               ).
      "No errors -> Display graph. Change constant to quickly switch between source code and diagram
    ELSEIF use_graph = abap_true.
      html = zcl_html_generator=>get_html_content_diagram(
                 iv_diagram_type = diagram_type
                 iv_object_name = object_name
                 iv_mermaid_code = diagram_result-mermaid_code
            ).
    ELSE.
      html = zcl_html_generator=>get_html_content_code(
                  iv_diagram_type = diagram_type
                  iv_object_name = object_name
                  iv_mermaid_code = diagram_result-mermaid_code
             ).
    ENDIF.

    "Display the HTML in a the Popup
    DATA(html_result) = cl_aia_result_factory=>create_html_popup_result( ).
    html_result->set_content( html ).
    result = html_result.
  ENDMETHOD.

  METHOD map_to_diagram_type.
    " This method maps the technical resource type provided by the IDE
    " to a type that zcl_diagram_generator is expecting.
    DATA(lv_object_type_uc) = to_upper( iv_object_type ).

    IF lv_object_type_uc CS 'BDEF'.
      rv_diagram_type = 'BDEF'.
    ELSEIF lv_object_type_uc CS 'CLAS'.
      rv_diagram_type = 'CLASS'.
    ELSEIF lv_object_type_uc CS 'DDLS'.
      rv_diagram_type = 'CDS'.
    ELSEIF lv_object_type_uc CS 'VIED'.
      rv_diagram_type = 'VIEW'.
    ELSEIF lv_object_type_uc CS 'INTF'.
      rv_diagram_type = 'INTF'.
    ELSE.
      " Case should never happen, because of the filters
      rv_diagram_type = ''.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
