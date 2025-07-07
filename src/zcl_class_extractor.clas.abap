CLASS zcl_class_extractor DEFINITION
  INHERITING FROM zcl_extractor_utilities
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Type definitions for class metadata structure
    TYPES:
      " Type to store data from one attribute
      BEGIN OF ty_attribute,
        attr_name   TYPE string,     " Attribute name
        attr_type   TYPE string,     " ABAP data type
        visibility  TYPE string,     " PUBLIC/PROTECTED/PRIVATE
        is_static   TYPE abap_bool,  " Static CLASS-DATA attribute flag
        is_constant TYPE abap_bool,  " Constant attribute flag
      END OF ty_attribute,
      " Type for all attributes of one class
      tt_attributes TYPE STANDARD TABLE OF ty_attribute WITH EMPTY KEY,

      " Type to store data from one method
      BEGIN OF ty_method,
        method_name TYPE string,     " Method name
        visibility  TYPE string,     " PUBLIC/PROTECTED/PRIVATE
        is_static   TYPE abap_bool,  " Static CLASS-METHOD method flag
        is_abstract TYPE abap_bool,  " Abstract method flag
      END OF ty_method,
      " Type for all methods of one class
      tt_methods TYPE STANDARD TABLE OF ty_method WITH EMPTY KEY,


      " Type to store all data related to one class/interface
      BEGIN OF ty_class_structure,
        class_name     TYPE string,      " Class name
        description    TYPE string,      " Class description
        superclass     TYPE string,      " Superclass name ("" if none)
        interfaces     TYPE string_table, " Implemented interfaces
        attributes     TYPE tt_attributes, " Attributes of class
        methods        TYPE tt_methods,   " Methods of class
        is_abstract    TYPE abap_bool,    " If class is abstract
        is_interface   TYPE abap_bool,   " If it is an interface
        friend_classes TYPE string_table, " Friend classes
        has_errors     TYPE abap_bool,    " Error flag
        error_info     TYPE ty_error_info, " Error details
      END OF ty_class_structure,
      " Type to store all classes, including the "starting" class
      tt_classes TYPE STANDARD TABLE OF ty_class_structure WITH EMPTY KEY.

    METHODS:
      " Parses the source file, and all "related" files and returns one big ty_class_structure
      get_class_info
        IMPORTING iv_class_name       TYPE string
                  iv_validate_access  TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rs_structure) TYPE ty_class_structure,

      " Validate class exists and is accessible
      validate_class
        IMPORTING iv_class_name   TYPE string
        EXPORTING ev_message        TYPE string
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      " Validate interface exists and is accessible
      validate_interface
        IMPORTING iv_interface_name TYPE string
        EXPORTING ev_message        TYPE string
        RETURNING VALUE(rv_valid)   TYPE abap_bool.

    INTERFACES zif_extractor.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS:
      " Extract all attributes of one class, using XCO_CLASS
      get_all_attributes
        IMPORTING lo_class             TYPE REF TO if_xco_ao_class
        RETURNING VALUE(rt_attributes) TYPE tt_attributes,

      " Extract methods of one class, using XCO_CLASS
      get_all_methods
        IMPORTING lo_class          TYPE REF TO if_xco_ao_class
        RETURNING VALUE(rt_methods) TYPE tt_methods,

      " Get a classes superclass name, using XCO_CLASS
      get_superclass_name
        IMPORTING io_superclass        TYPE REF TO if_xco_ao_class
        RETURNING VALUE(rv_superclass) TYPE string,

      " Get a classes interface names, using XCO_CLASS
      get_interface_names
        IMPORTING it_interfaces        TYPE sxco_t_ao_interfaces
        RETURNING VALUE(rt_interfaces) TYPE string_table,

      " Get a classes friends names, using XCO_CLASS
      get_friend_names
        IMPORTING it_friends        TYPE sxco_t_ao_classes
        RETURNING VALUE(rt_friends) TYPE string_table,

      " Get a interfaces methods, using XCO_INTF
      get_all_methods_intf
        IMPORTING lo_interface      TYPE REF TO if_xco_ao_interface
        RETURNING VALUE(rt_methods) TYPE tt_methods,

      " Get a interfaces attributes, using XCO_INTF
      get_all_attributes_intf
        IMPORTING lo_interface         TYPE REF TO if_xco_ao_interface
        RETURNING VALUE(rt_attributes) TYPE tt_attributes,

      " Method to convert the ty_class_structure to Mermaid Code
      generate_code_from_class_table
        IMPORTING lo_classes             TYPE tt_classes
        RETURNING VALUE(rt_mermaid_code) TYPE string,

      " Helper function to convert PRIVATE/PROTECTED/PUBLIC to -/#/+
      get_visibility_char
        IMPORTING lo_visibility             TYPE string
        RETURNING VALUE(rt_visibility_char) TYPE string.


ENDCLASS.

CLASS zcl_class_extractor IMPLEMENTATION.

  METHOD zif_extractor~generate_mermaid_code.
    DATA:
    " Keep track of a queue of classes still to process
    lt_queue     TYPE string_table,
    " Keep track of the already finished classes
    lt_finished  TYPE string_table,
    " The structure of all classes that were finished
    lt_classes   TYPE tt_classes,
    " Pointer to the next class
    lv_next_name TYPE string.

    " Start by adding the "root" class first
    APPEND iv_object_name TO lt_queue.

    " Do not stop until there are no classes that are left to process
    WHILE lines( lt_queue ) > 0.
      " Pop the class out of the queue
      READ TABLE lt_queue INTO lv_next_name INDEX 1.
      DELETE lt_queue INDEX 1.

      " Skip if already processed
      IF line_exists( lt_finished[ table_line = lv_next_name ] ).
        CONTINUE.
      ENDIF.

      TRY.
          " Extract the class info into a class structure and check if they were errors
          DATA(ls_class) = get_class_info( lv_next_name ).
          IF ls_class-has_errors = abap_true.
            rs_result-success = abap_false.
            rs_result-message = |Error while parsing class { ls_class-class_name }. Exited with error: "{ ls_class-error_info-error_message }"|.
            CONTINUE.
          ENDIF.

          " Add the finished structure into the table
          INSERT ls_class INTO lt_classes INDEX 1.

          " Add superclass of the processed class to the queue and only if it isn't enqueued yet or already finished.
          IF ls_class-superclass <> '' AND
            NOT line_exists( lt_finished[ table_line = ls_class-superclass ] ) AND
            NOT line_exists( lt_queue[ table_line = ls_class-superclass ] ).
            APPEND ls_class-superclass TO lt_queue.
          ENDIF.

          " Add interfaces of the processed class to the queue and only if it isn't enqueued yet or already finished.
          LOOP AT ls_class-interfaces INTO DATA(lv_iface).
            IF NOT line_exists( lt_finished[ table_line = lv_iface ] ) AND
               NOT line_exists( lt_queue[ table_line = lv_iface ] ).
              APPEND lv_iface TO lt_queue.
            ENDIF.
          ENDLOOP.

          " Add friends of the processed class to the queue and only if it isn't enqueued yet or already finished.
          LOOP AT ls_class-friend_classes INTO DATA(lv_friend).
            IF NOT line_exists( lt_finished[ table_line = lv_friend ] ) AND
               NOT line_exists( lt_queue[ table_line = lv_friend ] ).
              APPEND lv_friend TO lt_queue.
            ENDIF.
          ENDLOOP.

        " If there were any error -> set to false
        CATCH cx_root INTO DATA(lx_error).
          rs_result-success = abap_false.
          rs_result-message = |Exception while parsing class { ls_class-class_name } ({ lx_error->get_text(  ) })|.
          CONTINUE.
      ENDTRY.

      " Finally add it to the finished queue
      APPEND lv_next_name TO lt_finished.
    ENDWHILE.

    " Convert the table of classes to mermaid code
    rs_result-mermaid_code = generate_code_from_class_table( lt_classes ).
    rs_result-success = abap_true.
    rs_result-message = |Extracted { lines( lt_classes ) } class(es).|.
  ENDMETHOD.

  METHOD get_class_info.
    " CLEAR the rs_structure from last iteration
    CLEAR rs_structure.
    " Set name, based on passed variable
    rs_structure-class_name = iv_class_name.

    " Validate if it is a class
    DATA is_valid_class TYPE abap_bool.
    DATA lv_class_msg TYPE string.
    is_valid_class = validate_class(
        EXPORTING iv_class_name = iv_class_name
        IMPORTING ev_message = lv_class_msg
    ).

    " Validate if it is a interface
    DATA is_valid_interface TYPE abap_bool.
    DATA lv_interface_msg TYPE string.
    is_valid_interface = validate_interface(
      EXPORTING iv_interface_name = iv_class_name
      IMPORTING ev_message = lv_interface_msg
    ).

    " If both are false -> something went wrong
    IF is_valid_class = abap_false AND is_valid_interface = abap_false.
      " Return that there was an error and pass both error messages, since we do not know what was "really" the right type
      rs_structure-has_errors = abap_true.
      rs_structure-error_info-error_message = |Interface Error: { lv_interface_msg }\nClass Error: { lv_class_msg }|.
      RETURN.
    ENDIF.

    TRY.
        " XCO does not want a "normal" string -> convert it to the object name
        DATA(lv_name_c30) = CONV sxco_ao_object_name( iv_class_name ).

        " If it is a class, use the class related methods
        IF is_valid_class = abap_true.
          "Load the class using an extractor
          DATA(lo_class) = xco_cp_abap=>class( lv_name_c30 ).
          DATA(ls_class_content) = lo_class->definition->content( )->get( ).
          rs_structure-description    = | Class: { iv_class_name } |.
          " Set all the data of the class struct using the other methods
          rs_structure-superclass     = get_superclass_name( ls_class_content-superclass ).
          rs_structure-interfaces     = get_interface_names( ls_class_content-interfaces ).
          rs_structure-friend_classes = get_friend_names( ls_class_content-global_friends ).
          rs_structure-is_abstract    = ls_class_content-abstract_indicator.
          rs_structure-is_interface   = abap_false.
          rs_structure-methods        = get_all_methods( lo_class ).
          rs_structure-attributes     = get_all_attributes( lo_class ).
          rs_structure-has_errors     = abap_false.

        " If it is an interface, use the interface related methods
        ELSEIF is_valid_interface = abap_true.
          "Load the interface using an extractor
          DATA(lo_interface) = xco_cp_abap=>interface( lv_name_c30 ).
          DATA(ls_interface_content) = lo_interface->content(  )->get(  ).
          rs_structure-description    = | Interface: { iv_class_name } |.
          rs_structure-superclass     = ''.
          " Set all the data of the class struct using the other methods
          rs_structure-interfaces     = get_interface_names( ls_interface_content-interfaces ).
          rs_structure-friend_classes = VALUE string_table( ). " Can't have friends
          rs_structure-is_abstract    = abap_false.
          rs_structure-is_interface   = abap_true.
          rs_structure-attributes     = get_all_attributes_intf( lo_interface ).
          rs_structure-methods        = get_all_methods_intf( lo_interface ).
          rs_structure-has_errors     = abap_false.
        ELSE.
          rs_structure-has_errors = abap_true.
          rs_structure-error_info-error_message = |Type is not interface, not class|.
        ENDIF.

      CATCH cx_root.
        rs_structure-has_errors = abap_true.
        rs_structure-error_info-error_message = |Exception occured during generation.|.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_all_attributes.
    DATA lt_attributes TYPE tt_attributes.

    " XCO groups every attribute under the access modifiers, so we loop over every access modifier one at a time
    LOOP AT VALUE string_table( ( |PUBLIC| ) ( |PROTECTED| ) ( |PRIVATE| ) ) INTO DATA(lv_vis).
      " Select the right "section" per iteration
      DATA(lo_section) = SWITCH #(
        lv_vis
        WHEN 'PRIVATE'   THEN lo_class->definition->section-private
        WHEN 'PROTECTED' THEN lo_class->definition->section-protected
        WHEN 'PUBLIC'    THEN lo_class->definition->section-public
      ).

      " Loop over every DATA and add it to the array
      LOOP AT lo_section->components->data->all->get( ) INTO DATA(lo_data).
        APPEND VALUE ty_attribute(
          attr_name  = lo_data->name " Get the data name
          attr_type  = lo_data->content(  )->get_typing_definition(  )->get_value(  ) " Get the data type
          visibility = lv_vis " Set the visibility based on the current loop iteration
          is_static  = abap_false " Not CLASS-DATA -> not static
          is_constant = abap_false " Not a constant
        ) TO lt_attributes.
      ENDLOOP.

      " Loop over every CLASS-DATA and add it to the array
      LOOP AT lo_section->components->class_data->all->get(  ) INTO DATA(lo_class_data).
        APPEND VALUE ty_attribute(
          attr_name  = lo_class_data->name " Get the data name
          attr_type  = lo_class_data->content(  )->get_typing_definition(  )->get_value(  ) " Get the data type
          visibility = lv_vis " Set the visibility based on the current loop iteration
          is_static  = abap_true " CLASS-DATA -> static
          is_constant = abap_false " Not a constant
        ) TO lt_attributes.
      ENDLOOP.

      " Loop over every constant and add it to the array
      LOOP AT lo_section->components->constant->all->get(  ) INTO DATA(lo_const).
        APPEND VALUE ty_attribute(
          attr_name  = lo_const->name " Get the data name
          attr_type  = lo_const->content(  )->get_typing_definition(  )->get_value(  ) " Get the data type
          visibility = lv_vis " Set the visibility based on the current loop iteration
          is_static  = abap_true " Constants are treated as static here
          is_constant = abap_true " Constant -> true
        ) TO lt_attributes.
      ENDLOOP.
    ENDLOOP.

    RETURN lt_attributes.
  ENDMETHOD.

  METHOD get_all_methods.
    DATA lt_all_methods TYPE tt_methods.

    " XCO groups every method under the access modifiers, so we loop over every access modifier one at a time
    LOOP AT VALUE string_table( ( |PUBLIC| ) ( |PROTECTED| ) ( |PRIVATE| ) ) INTO DATA(lv_vis).
      " Select the right "section" per iteration
      DATA(lo_section) = SWITCH #(
        lv_vis
        WHEN 'PRIVATE'   THEN lo_class->definition->section-private
        WHEN 'PROTECTED' THEN lo_class->definition->section-protected
        WHEN 'PUBLIC'    THEN lo_class->definition->section-public
      ).

      " Loop over all methods and append it to the array
      LOOP AT lo_section->components->method->all->get(  ) INTO DATA(lo_method).
        APPEND VALUE ty_method(
          method_name = lo_method->name " Get the method name
          visibility  = lv_vis " Set the visibility based on the current loop iteration
          is_static   = abap_false " Normal methods are not static
          is_abstract = lo_method->content(  )->get_abstract_indicator(  ) " If the method is abstract
        ) TO lt_all_methods.
      ENDLOOP.

      " Loop over all CLASS-Methods and append it to the array
      LOOP AT lo_section->components->class_method->all->get(  ) INTO DATA(lo_class_method).
        APPEND VALUE ty_method(
          method_name = lo_class_method->name " Get the method name
          visibility  = lv_vis " Set the visibility based on the current loop iteration
          is_static   = abap_true " CLASS-METHODs are not static
          is_abstract = lo_class_method->content(  )->get_abstract_indicator(  ) " If the method is abstract
        ) TO lt_all_methods.
      ENDLOOP.
    ENDLOOP.

    RETURN lt_all_methods.
  ENDMETHOD.

  METHOD get_superclass_name.
    " If there is a superclass, add it, if not set it to the empty string
    IF io_superclass IS BOUND.
      rv_superclass = io_superclass->name.
    ELSE.
      rv_superclass = ''.
    ENDIF.
  ENDMETHOD.

  METHOD get_interface_names.
    " Go over the interface structure and add the name, if it is existent
    LOOP AT it_interfaces INTO DATA(lo_interface).
      IF lo_interface IS BOUND.
        APPEND lo_interface->name TO rt_interfaces.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_friend_names.
    " Go over the friend structure and add the name, if it is existent
    LOOP AT it_friends INTO DATA(lo_friend).
      IF lo_friend IS BOUND.
        APPEND lo_friend->name TO rt_friends.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_class.
    " Start with false and only set to true, if everything "went well"
    rv_valid = abap_false.
    ev_message = ''.

    TRY.
        " Check if the name follows the SAP naming rules
        IF iv_class_name IS INITIAL OR strlen( iv_class_name ) > 30.
          ev_message = 'Class name is empty or too long.'.
          RETURN.
        ENDIF.

        " Use XCO to validate class view existence
        DATA(lv_name_c30) = CONV sxco_ao_object_name( iv_class_name ).
        DATA(lo_view_entity) = xco_cp_abap=>class( lv_name_c30 ).

        " Check if there are syntax errors in the class
        IF lo_view_entity->check_syntax(  )->passed = abap_true.
          rv_valid = abap_true.
        ELSE.
          ev_message = |Syntax check failed for { iv_class_name }|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch all errors and "reset" rv_valid and set the error message to the exceptions'.
        rv_valid = abap_false.
        ev_message = lx_error->get_text(  ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_all_attributes_intf.
    DATA lt_attributes TYPE tt_attributes.

    " Go over every data of the interface and convert each to the attribute struct
    LOOP AT lo_interface->components->data->all->get( ) INTO DATA(lo_data).
      APPEND VALUE ty_attribute(
        attr_name  = lo_data->name " The name of the data
        attr_type  = lo_data->content(  )->get_typing_definition(  )->get_value(  ) " The name of the data type
        visibility = 'PUBLIC' " By nature all data in interfaces are public
        is_static  = abap_false " Not CLASS-DATA -> not static
        is_constant = abap_false " Not a constant -> false
      ) TO lt_attributes.
    ENDLOOP.

    " Go over every CLASS-Data of the interface and convert each to the attribute struct
    LOOP AT lo_interface->components->class_data->all->get(  ) INTO DATA(lo_class_data).
      APPEND VALUE ty_attribute(
        attr_name  = lo_class_data->name " The name of the class data
        attr_type  = lo_class_data->content(  )->get_typing_definition(  )->get_value(  ) " The name of the data type
        visibility = 'PUBLIC' " By nature all data in interfaces are public
        is_static  = abap_true " CLASS-DATA -> static
        is_constant = abap_false " Not a constant -> false
      ) TO lt_attributes.
    ENDLOOP.

    " Go over every constant of the interface and convert each to the attribute struct
    LOOP AT lo_interface->components->constant->all->get(  ) INTO DATA(lo_const).
      APPEND VALUE ty_attribute(
        attr_name  = lo_const->name " The name of the class data
        attr_type  = lo_const->content(  )->get_typing_definition(  )->get_value(  ) " The name of the data type
        visibility = 'PUBLIC' " By nature all data in interfaces are public
        is_static  = abap_true " Constants are the same for every instance of the interface -> static
        is_constant = abap_true " Constants -> true
      ) TO lt_attributes.
    ENDLOOP.

    RETURN lt_attributes.
  ENDMETHOD.

  METHOD get_all_methods_intf.
    DATA lt_all_methods TYPE tt_methods.

    " Go over every method and for each add the method struct to the array
    LOOP AT lo_interface->components->method->all->get(  ) INTO DATA(lo_method).
      APPEND VALUE ty_method(
        method_name = lo_method->name
        visibility  = 'PUBLIC' "By nature all interface methods are public.
        is_static   = abap_false "No CLASS-METHOD -> not static
        is_abstract = abap_true "By nature all interface methods are abstract.
      ) TO lt_all_methods.
    ENDLOOP.

    " Go over every CLASS-Method and for each add the method struct to the array
    LOOP AT lo_interface->components->class_method->all->get(  ) INTO DATA(lo_class_method).
      APPEND VALUE ty_method(
        method_name = lo_class_method->name
        visibility  = 'PUBLIC' "By nature all interface methods are public.
        is_static   = abap_true "CLASS-METHOD -> static
        is_abstract = abap_false "By nature all interface methods are abstract.
      ) TO lt_all_methods.
    ENDLOOP.

    RETURN lt_all_methods.
  ENDMETHOD.

  METHOD validate_interface.
    " Start with false and only set to true, if everything "went well"
    rv_valid = abap_false.
    ev_message = ''.

    TRY.
        " Check if the name follows the SAP naming rules
        IF iv_interface_name IS INITIAL OR strlen( iv_interface_name ) > 30.
          ev_message = 'Interface name is empty or too long.'.
          RETURN.
        ENDIF.

        " Try to find the object by using XCO and catch any error that comes up.
        DATA(lv_name_c30) = CONV sxco_ao_object_name( iv_interface_name ).
        DATA(lo_view_entity) = xco_cp_abap=>interface( lv_name_c30 ).

        " Using XCO, check if the syntax is correct
        IF lo_view_entity->check_syntax( )->passed = abap_true.
          rv_valid = abap_true.
        ELSE.
          ev_message = |Syntax check failed for { iv_interface_name }|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Catch all errors and "reset" rv_valid and set the error message to the exceptions'.
        rv_valid = abap_false.
        ev_message = lx_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD generate_code_from_class_table.
    DATA(lv_mermaid) = |classDiagram\n|.

    LOOP AT lo_classes INTO DATA(ls_class).
      "General class attributes
      lv_mermaid = lv_mermaid && |    class { ls_class-class_name } \{\n|.
      IF ls_class-is_abstract = abap_true.
        lv_mermaid = lv_mermaid && |    << abstract >>\n|.
      ENDIF.
      IF ls_class-is_interface = abap_true.
        lv_mermaid = lv_mermaid && |    <<  interface  >>\n|.
      ENDIF.

      " Add attributes
      LOOP AT ls_class-attributes INTO DATA(ls_attribute).
        DATA(visibility_modifier_attr) = get_visibility_char( ls_attribute-visibility ).
        DATA(constant_attribute) = COND string( WHEN ls_attribute-is_constant = abap_true THEN 'const' ELSE '' ).
        DATA(static_attribute) = COND  string( WHEN ls_attribute-is_static = abap_true THEN '$' ELSE '' ).
        " Build the final Mermaid line using the attributes found above
        lv_mermaid = lv_mermaid && |    { visibility_modifier_attr } { constant_attribute } { ls_attribute-attr_name }: { ls_attribute-attr_type }{ static_attribute }\n|.
      ENDLOOP.
      " Add methods
      LOOP AT ls_class-methods INTO DATA(ls_method).
        DATA(visibility_modifier_method) = get_visibility_char( ls_method-visibility ).
        DATA(abstract_method) = COND string( WHEN ls_method-is_abstract = abap_true THEN '*' ELSE '' ).
        DATA(static_method) = COND  string( WHEN ls_method-is_static = abap_true THEN '$' ELSE '' ).
        " Build the final Mermaid line using the attributes found above
        lv_mermaid = lv_mermaid && |    { visibility_modifier_method } { ls_method-method_name }(){ abstract_method }{ static_method }\n|.
      ENDLOOP.
      "End class definition
      lv_mermaid = lv_mermaid && |    \}\n|.

      " Add relationships:
      IF ls_class-superclass <> ''.
        lv_mermaid = lv_mermaid && |    { ls_class-class_name } --\|> { ls_class-superclass }\n|.
      ENDIF.
      LOOP AT ls_class-friend_classes INTO DATA(ls_friend).
        lv_mermaid = lv_mermaid && |    { ls_class-class_name } <-- { ls_friend }: Friend of\n|.
      ENDLOOP.
      LOOP AT ls_class-interfaces INTO DATA(ls_interface).
        lv_mermaid = lv_mermaid && |    { ls_class-class_name } ..\|> { ls_interface }\n|.
      ENDLOOP.
    ENDLOOP.
    rt_mermaid_code = lv_mermaid.
  ENDMETHOD.

  METHOD get_visibility_char.
    IF lo_visibility = |PRIVATE|.
      RETURN '-'.
    ELSEIF lo_visibility = |PROTECTED|.
      RETURN '#'.
    ELSEIF lo_visibility = |PUBLIC|.
      RETURN '+'.
    ELSE.
      RETURN '?'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
