CLASS zcl_behavior_extractor DEFINITION
  INHERITING FROM zcl_extractor_utilities
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Type definitions for BDEF metadata structure
    TYPES: BEGIN OF ty_entity,
             entity_name   TYPE string,
             alias         TYPE string,
             is_root       TYPE abap_bool,
             draft_enabled TYPE abap_bool,
             lock_master   TYPE string,
             etag_field    TYPE string,
             cds_view      TYPE string,
             persistent_table TYPE string,
             implementation_class TYPE string,
             readonly_fields TYPE string_table,
           END OF ty_entity.
"test
    TYPES tt_entities TYPE STANDARD TABLE OF ty_entity WITH EMPTY KEY.

    TYPES: BEGIN OF ty_operation,
             entity_name TYPE string,
             operation   TYPE string,    " CREATE, UPDATE, DELETE, etc.
             is_enabled  TYPE abap_bool,
             options     TYPE string,    " Additional operation options
           END OF ty_operation.

    TYPES tt_operations TYPE STANDARD TABLE OF ty_operation WITH EMPTY KEY.

    TYPES: BEGIN OF ty_association,
             parent_entity    TYPE string,
             child_entity     TYPE string,
             association_name TYPE string,
             association_type TYPE string, " ASSOCIATION, COMPOSITION
             has_create       TYPE abap_bool,
             cardinality      TYPE string,
           END OF ty_association.

    TYPES tt_associations TYPE STANDARD TABLE OF ty_association WITH EMPTY KEY.

    TYPES: BEGIN OF ty_field_mapping,
             entity_name TYPE string,
             cds_field   TYPE string,
             table_field TYPE string,
           END OF ty_field_mapping.

    TYPES tt_field_mappings TYPE STANDARD TABLE OF ty_field_mapping WITH EMPTY KEY.

    TYPES: BEGIN OF ty_bdef_structure,
             bdef_name    TYPE string,
             description  TYPE string,
             implementation_type TYPE string, " managed, unmanaged
             implementation_class TYPE string,
             entities     TYPE tt_entities,
             operations   TYPE tt_operations,
             associations TYPE tt_associations,
             field_mappings TYPE tt_field_mappings,
             has_errors   TYPE abap_bool,
             error_info   TYPE ty_error_info,
           END OF ty_bdef_structure.

    " Extract BDEF metadata
    METHODS: extract_bdef_metadata
      IMPORTING iv_bdef_name        TYPE string
      RETURNING VALUE(rs_structure) TYPE ty_bdef_structure,

      " Validate BDEF exists and is accessible
      validate_bdef
        IMPORTING iv_bdef_name    TYPE string
        RETURNING VALUE(rv_valid) TYPE abap_bool.

    INTERFACES zif_extractor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    " Get BDEF source using XCO (Cloud-compatible)
    METHODS: get_bdef_source_xco
      IMPORTING iv_bdef_name      TYPE string
      RETURNING VALUE(rt_source)  TYPE string_table
      RAISING   cx_root,

      " Parse BDEF header information
      parse_bdef_header
        IMPORTING it_source TYPE string_table
        CHANGING  cs_structure TYPE ty_bdef_structure,

      " Enhanced entity parsing
      parse_entities_enhanced
        IMPORTING it_source         TYPE string_table
        RETURNING VALUE(rt_entities) TYPE tt_entities,

      " Parse operations from source
      parse_operations_from_source
        IMPORTING it_source           TYPE string_table
                  it_entities         TYPE tt_entities
        RETURNING VALUE(rt_operations) TYPE tt_operations,

      " Parse associations from source
      parse_associations_from_source
        IMPORTING it_source             TYPE string_table
                  it_entities           TYPE tt_entities
        RETURNING VALUE(rt_associations) TYPE tt_associations,

      " Parse field mappings
      parse_field_mappings
        IMPORTING it_source TYPE string_table
                  it_entities TYPE tt_entities
        RETURNING VALUE(rt_mappings) TYPE tt_field_mappings,

      " Helper methods
      get_standard_operations
        RETURNING VALUE(rt_operations) TYPE string_table,

      clean_entity_name
        IMPORTING iv_raw_name TYPE string
        RETURNING VALUE(rv_clean_name) TYPE string.

ENDCLASS.

CLASS zcl_behavior_extractor IMPLEMENTATION.

  METHOD zif_extractor~generate_mermaid_code.
    " Extract BDEF metadata
    DATA(ls_bdef) = extract_bdef_metadata( iv_object_name ).

    " Check if structure has errors
    IF ls_bdef-has_errors = abap_true.
      rs_result-success = abap_false.
      rs_result-message = ls_bdef-error_info-error_message.
      RETURN.
    ENDIF.

    " Check if we have entities
    IF lines( ls_bdef-entities ) = 0.
      rs_result-success = abap_false.
      rs_result-message = |No entities found in behavior definition '{ iv_object_name }'|.
      RETURN.
    ENDIF.

    " Generate enhanced Mermaid class diagram
    DATA(lv_mermaid) = |classDiagram\n|.

    " Add BDEF header information as notes
    IF ls_bdef-implementation_type IS NOT INITIAL.
      lv_mermaid = lv_mermaid && |    note "Implementation: { ls_bdef-implementation_type }"\n|.
    ENDIF.
    IF ls_bdef-implementation_class IS NOT INITIAL.
      lv_mermaid = lv_mermaid && |    note "Class: { ls_bdef-implementation_class }"\n|.
    ENDIF.
    lv_mermaid = lv_mermaid && |\n|.

    " Define entities as classes with enhanced information
    LOOP AT ls_bdef-entities INTO DATA(ls_entity) WHERE entity_name IS NOT INITIAL.
      DATA(lv_class_name) = ls_entity-entity_name.

      " Add entity class
      lv_mermaid = lv_mermaid && |    class { lv_class_name } \{\n|.

      " Add stereotype based on root/child
      IF ls_entity-is_root = abap_true.
        lv_mermaid = lv_mermaid && |        <<Root Entity>>\n|.
      ELSE.
        lv_mermaid = lv_mermaid && |        <<Child Entity>>\n|.
      ENDIF.

      " Add entity properties
      IF ls_entity-alias IS NOT INITIAL AND ls_entity-alias <> ls_entity-entity_name.
        lv_mermaid = lv_mermaid && |        +alias: { ls_entity-alias }\n|.
      ENDIF.

      IF ls_entity-persistent_table IS NOT INITIAL.
        lv_mermaid = lv_mermaid && |        +persistentTable: { ls_entity-persistent_table }\n|.
      ENDIF.

      IF ls_entity-cds_view IS NOT INITIAL.
        lv_mermaid = lv_mermaid && |        +cdsView: { ls_entity-cds_view }\n|.
      ENDIF.

      IF ls_entity-lock_master IS NOT INITIAL.
        lv_mermaid = lv_mermaid && |        +lockType: { ls_entity-lock_master }\n|.
      ENDIF.

      IF ls_entity-draft_enabled = abap_true.
        lv_mermaid = lv_mermaid && |        +draftEnabled: true\n|.
      ENDIF.

      " Add readonly fields if any
      IF lines( ls_entity-readonly_fields ) > 0.
        lv_mermaid = lv_mermaid && |        ---\n|.
        LOOP AT ls_entity-readonly_fields INTO DATA(lv_readonly_field).
          lv_mermaid = lv_mermaid && |        { lv_readonly_field }: readonly\n|.
        ENDLOOP.
      ENDIF.

      " Add operations section
      DATA(lv_has_operations) = abap_false.
      LOOP AT ls_bdef-operations INTO DATA(ls_operation) WHERE entity_name = ls_entity-entity_name.
        IF lv_has_operations = abap_false.
          lv_mermaid = lv_mermaid && |        ---\n|.
          lv_has_operations = abap_true.
        ENDIF.

        DATA(lv_operation_display) = ls_operation-operation.
        IF ls_operation-options IS NOT INITIAL.
          lv_operation_display = |{ ls_operation-operation }({ ls_operation-options })|.
        ENDIF.

        IF ls_operation-is_enabled = abap_true.
          lv_mermaid = lv_mermaid && |        +{ lv_operation_display }()\n|.
        ELSE.
          lv_mermaid = lv_mermaid && |        -{ lv_operation_display }() [DISABLED]\n|.
        ENDIF.
      ENDLOOP.

      lv_mermaid = lv_mermaid && |    \}\n\n|.
    ENDLOOP.

    " Add relationships/associations
    LOOP AT ls_bdef-associations INTO DATA(ls_association) WHERE parent_entity IS NOT INITIAL.
      DATA(lv_relationship) = COND string(
        WHEN ls_association-association_type = 'COMPOSITION'
        THEN '*--'  " Composition
        ELSE '-->'  " Association
      ).

      DATA(lv_label) = ls_association-association_name.
      IF ls_association-has_create = abap_true.
        lv_label = |{ lv_label } [create]|.
      ENDIF.
      IF ls_association-cardinality IS NOT INITIAL.
        lv_label = |{ lv_label } ({ ls_association-cardinality })|.
      ENDIF.

      lv_mermaid = lv_mermaid && |    { ls_association-parent_entity } { lv_relationship } { ls_association-child_entity } : "{ lv_label }"\n|.
    ENDLOOP.

    rs_result-success = abap_true.
    rs_result-mermaid_code = lv_mermaid.
  ENDMETHOD.

  METHOD extract_bdef_metadata.
    " Initialize result structure
    CLEAR rs_structure.
    rs_structure-bdef_name = iv_bdef_name.

    " Explicit declarations
    DATA lt_source TYPE string_table.
    DATA lx_root   TYPE REF TO cx_root.
    DATA ls_bdef_temp TYPE ty_bdef_structure. " For parsing into a temporary structure

    TRY.

        " Get BDEF source code using XCO
        lt_source = get_bdef_source_xco( iv_bdef_name ).

        " Parse BDEF components
        " Create a temporary structure for parsing, then copy to rs_structure
        ls_bdef_temp = rs_structure. " Initialize with current bdef_name

        parse_bdef_header(
          EXPORTING it_source = lt_source
          CHANGING cs_structure = ls_bdef_temp
        ).

        ls_bdef_temp-entities = parse_entities_enhanced( lt_source ).
        ls_bdef_temp-operations = parse_operations_from_source(
          it_source = lt_source
          it_entities = ls_bdef_temp-entities
        ).
        ls_bdef_temp-associations = parse_associations_from_source(
          it_source = lt_source
          it_entities = ls_bdef_temp-entities
        ).
        ls_bdef_temp-field_mappings = parse_field_mappings(
          it_source = lt_source
          it_entities = ls_bdef_temp-entities
        ).

        ls_bdef_temp-has_errors = abap_false.

        " Copy the parsed data back to the return structure
        rs_structure = ls_bdef_temp.

      CATCH cx_root INTO lx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_bdef.
    rv_valid = abap_false.

    TRY.
        " Basic validation
        IF iv_bdef_name IS INITIAL OR strlen( iv_bdef_name ) > 30.
          RETURN.
        ENDIF.

        " Use XCO to validate behavior definition existence (Cloud-compatible)
        DATA(lo_behavior_definition) = xco_cp_abap_repository=>object->bdef->for(
          CONV sxco_cds_object_name( iv_bdef_name )
        ).

        IF lo_behavior_definition->exists( ) = abap_true.
          rv_valid = abap_true.
        ENDIF.

      CATCH cx_root.
        rv_valid = abap_false.
    ENDTRY.
  ENDMETHOD.


METHOD get_bdef_source_xco.
    CLEAR rt_source.
    TRY.
        " Using XCO_CP_ABAP_REPOSITORY with different read access
        DATA(lo_repo_object) = xco_cp_abap_repository=>object->bdef->for(
          CONV sxco_cds_object_name( iv_bdef_name )
        ).

      CATCH cx_root.

                " Using CDS module for behavior definition access
                TRY.
                    DATA(lo_cds_bdef) = xco_cp_cds=>behavior_definition(
                      iv_name = CONV sxco_cds_object_name( iv_bdef_name )
                    ).

                    " Try different content access methods
                    DATA(lo_bdef_content) = lo_cds_bdef->content( ).

                  CATCH cx_root.
                ENDTRY.
            ENDTRY.

  ENDMETHOD.

  METHOD parse_bdef_header.
    " Parse the first line for implementation type and class
    IF lines( it_source ) > 0.
      READ TABLE it_source INDEX 1 INTO DATA(lv_first_line).
      DATA(lv_line_upper) = to_upper( lv_first_line ).

      " Check for managed/unmanaged
      IF lv_line_upper CS 'MANAGED'.
        cs_structure-implementation_type = 'managed'.
      ELSEIF lv_line_upper CS 'UNMANAGED'.
        cs_structure-implementation_type = 'unmanaged'.
      ENDIF.

      " Extract implementation class
      FIND REGEX 'CLASS\s+(\w+)' IN lv_line_upper SUBMATCHES DATA(lv_class).
      IF lv_class IS NOT INITIAL.
        cs_structure-implementation_class = lv_class.
      ENDIF.
    ENDIF.

    cs_structure-description = |Behavior Definition: { cs_structure-bdef_name }|.
  ENDMETHOD.

METHOD parse_entities_enhanced.
    DATA: ls_entity TYPE ty_entity,
          lv_in_entity TYPE abap_bool.
    DATA: lv_line TYPE string,
          lv_current_line TYPE string,
          lv_line_upper TYPE string.
    DATA: lv_define_pos TYPE i,
          lv_start_define_offset TYPE i,
          lv_remainder TYPE string,
          lv_alias_pos TYPE i,
          lv_start_alias_offset TYPE i,
          lv_entity_part TYPE string,
          lv_alias_part TYPE string.
    DATA: lv_table_pos TYPE i,
          lv_start_table_offset TYPE i,
          lv_table_name_part TYPE string.
    DATA: lv_paren_open_pos TYPE i,
          lv_paren_close_pos TYPE i,
          lv_start_extract_offset TYPE i,
          lv_extract_length TYPE i,
          lv_fields_text TYPE string,
          lt_field_parts TYPE string_table,
          lv_field_part TYPE string,
          lv_clean_field TYPE string.

    CLEAR rt_entities.

    LOOP AT it_source INTO lv_line.
      lv_current_line = condense( lv_line ).
      lv_line_upper = to_upper( lv_current_line ).

      " Look for entity definitions
      IF lv_line_upper CS 'DEFINE BEHAVIOR FOR'.
        CLEAR ls_entity. " Clear for new entity
        lv_in_entity = abap_true.

        " Extract entity name and alias
        FIND 'DEFINE BEHAVIOR FOR' IN lv_line_upper MATCH OFFSET lv_define_pos.
        IF sy-subrc = 0.
          " Calculate the start offset explicitly
          lv_start_define_offset = lv_define_pos + strlen( 'DEFINE BEHAVIOR FOR' ).
          lv_remainder = lv_line_upper+lv_start_define_offset.
          CONDENSE lv_remainder.

          FIND 'ALIAS' IN lv_remainder MATCH OFFSET lv_alias_pos.
          IF sy-subrc = 0.
            lv_entity_part = lv_remainder(lv_alias_pos).
            " Calculate the start offset explicitly for alias part
            lv_start_alias_offset = lv_alias_pos + strlen( 'ALIAS' ).
            lv_alias_part = lv_remainder+lv_start_alias_offset.
            CONDENSE: lv_entity_part, lv_alias_part.
            ls_entity-entity_name = clean_entity_name( lv_entity_part ).
            ls_entity-alias = clean_entity_name( lv_alias_part ).
          ELSE.
            CONDENSE lv_remainder.
            ls_entity-entity_name = clean_entity_name( lv_remainder ).
            ls_entity-alias = ls_entity-entity_name.
          ENDIF.
          ls_entity-cds_view = ls_entity-entity_name.
        ENDIF.

      " Parse entity properties
      ELSEIF lv_in_entity = abap_true.
        " Persistent table
        IF lv_line_upper CS 'PERSISTENT TABLE'.
          FIND 'PERSISTENT TABLE' IN lv_line_upper MATCH OFFSET lv_table_pos.
          IF sy-subrc = 0.
            " Calculate the start offset explicitly
            lv_start_table_offset = lv_table_pos + strlen( 'PERSISTENT TABLE' ).
            lv_table_name_part = lv_line_upper+lv_start_table_offset.
            CONDENSE lv_table_name_part.
            " Remove any trailing characters like ';' or '{'
            REPLACE ALL OCCURRENCES OF REGEX '[\s;\{].*$' IN lv_table_name_part WITH ''.
            ls_entity-persistent_table = clean_entity_name( lv_table_name_part ).
          ENDIF.

        " Lock master/dependent
        ELSEIF lv_line_upper CS 'LOCK MASTER'.
          ls_entity-is_root = abap_true.
          ls_entity-lock_master = 'MASTER'.
        ELSEIF lv_line_upper CS 'LOCK DEPENDENT'.
          ls_entity-is_root = abap_false.
          ls_entity-lock_master = 'DEPENDENT'.

        " Draft
        ELSEIF lv_line_upper CS 'DRAFT'.
          ls_entity-draft_enabled = abap_true.

        " Field definitions (readonly)
        ELSEIF lv_line_upper CS 'FIELD' AND lv_line_upper CS 'READONLY'.
          FIND '(' IN lv_current_line MATCH OFFSET lv_paren_open_pos.
          IF sy-subrc = 0. " Changed SY-RC to SY-SUBRC
            FIND ')' IN lv_current_line MATCH OFFSET lv_paren_close_pos.
            IF sy-subrc = 0 AND lv_paren_close_pos > lv_paren_open_pos.
              lv_start_extract_offset = lv_paren_open_pos + 1.
              lv_extract_length = lv_paren_close_pos - lv_paren_open_pos - 1.

              IF lv_extract_length < 0. lv_extract_length = 0. ENDIF.
              lv_fields_text = lv_current_line+lv_start_extract_offset(lv_extract_length).
              SPLIT lv_fields_text AT ',' INTO TABLE lt_field_parts.
              LOOP AT lt_field_parts INTO lv_field_part.
                lv_clean_field = condense( lv_field_part ).
                REPLACE ALL OCCURRENCES OF ';' IN lv_clean_field WITH ''.
                IF lv_clean_field IS NOT INITIAL.
                  APPEND lv_clean_field TO ls_entity-readonly_fields.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

        " End of entity definition
        ELSEIF lv_line_upper = '}' AND ls_entity-entity_name IS NOT INITIAL.
          APPEND ls_entity TO rt_entities.
          lv_in_entity = abap_false.
          CLEAR ls_entity. " Clear for next entity
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ls_entity-entity_name IS NOT INITIAL.
      APPEND ls_entity TO rt_entities.
    ENDIF.
  ENDMETHOD.

  METHOD parse_operations_from_source.
    DATA: ls_operation TYPE ty_operation,
          lv_current_entity TYPE string,
          lv_in_entity TYPE abap_bool.
    DATA: lv_line TYPE string,
          lv_line_upper TYPE string.
    DATA: lv_define_pos TYPE i,
          lv_remainder TYPE string,
          lv_dummy TYPE string.

    CLEAR rt_operations.

    LOOP AT it_source INTO lv_line.
      lv_line_upper = to_upper( condense( lv_line ) ).

      " Track current entity
      IF lv_line_upper CS 'DEFINE BEHAVIOR FOR'.
        FIND 'DEFINE BEHAVIOR FOR' IN lv_line_upper MATCH OFFSET lv_define_pos.
        IF sy-subrc = 0.
          DATA(lv_start_define_offset) = lv_define_pos + strlen( 'DEFINE BEHAVIOR FOR' ).
          lv_remainder = lv_line_upper+lv_start_define_offset.
          CONDENSE lv_remainder.
          SPLIT lv_remainder AT space INTO lv_current_entity lv_dummy.
          lv_in_entity = abap_true.
        ENDIF.

      " Look for operations within entity
      ELSEIF lv_in_entity = abap_true AND lv_current_entity IS NOT INITIAL.
        " Check for standard operations and actions (not in this scope, but good to note)
        IF lv_line_upper CS 'CREATE;'. " Use CS for robustness
          APPEND VALUE ty_operation(
            entity_name = lv_current_entity
            operation = 'CREATE'
            is_enabled = abap_true
          ) TO rt_operations.

        ELSEIF lv_line_upper CS 'UPDATE;'.
          APPEND VALUE ty_operation(
            entity_name = lv_current_entity
            operation = 'UPDATE'
            is_enabled = abap_true
          ) TO rt_operations.

        ELSEIF lv_line_upper CS 'DELETE;'.
          APPEND VALUE ty_operation(
            entity_name = lv_current_entity
            operation = 'DELETE'
            is_enabled = abap_true
          ) TO rt_operations.
        ENDIF.

      " End of entity
      ELSEIF lv_line_upper = '}' AND lv_in_entity = abap_true.
        lv_in_entity = abap_false.
        CLEAR lv_current_entity.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_associations_from_source.
    DATA: ls_association TYPE ty_association,
          lv_current_entity TYPE string,
          lv_in_entity TYPE abap_bool.
    DATA: lv_line TYPE string,
          lv_line_upper TYPE string.
    DATA: lv_define_pos TYPE i,
          lv_remainder TYPE string,
          lv_dummy TYPE string,
          lv_assoc_pos TYPE i,
          lv_assoc_part TYPE string,
          lv_assoc_name TYPE string,
          lv_rest_of_line TYPE string,
          lv_potential_child_name TYPE string,
          ls_found_entity TYPE ty_entity,
          lv_cardinality_match TYPE string.

    CLEAR rt_associations.

    LOOP AT it_source INTO lv_line.
      lv_line_upper = to_upper( condense( lv_line ) ).

      " Track current entity
      IF lv_line_upper CS 'DEFINE BEHAVIOR FOR'.
        FIND 'DEFINE BEHAVIOR FOR' IN lv_line_upper MATCH OFFSET lv_define_pos.
        IF sy-subrc = 0.
          DATA(lv_start_define_offset) = lv_define_pos + strlen( 'DEFINE BEHAVIOR FOR' ).
          lv_remainder = lv_line_upper+lv_start_define_offset.
          CONDENSE lv_remainder.
          SPLIT lv_remainder AT space INTO lv_current_entity lv_dummy.
          lv_in_entity = abap_true.
        ENDIF.

      " Look for associations within entity
      ELSEIF lv_in_entity = abap_true AND lv_current_entity IS NOT INITIAL.
        IF lv_line_upper CS 'ASSOCIATION'.
          FIND 'ASSOCIATION' IN lv_line_upper MATCH OFFSET lv_assoc_pos.
          IF sy-subrc = 0.
            DATA(lv_start_assoc_offset) = lv_assoc_pos + strlen( 'ASSOCIATION' ).
            lv_assoc_part = lv_line_upper+lv_start_assoc_offset.
            CONDENSE lv_assoc_part.
            " Extract association name (first word)
            SPLIT lv_assoc_part AT space INTO lv_assoc_name lv_rest_of_line.

            IF lv_assoc_name IS NOT INITIAL.
              ls_association-parent_entity = lv_current_entity.
              ls_association-association_name = lv_assoc_name.
              ls_association-association_type = 'ASSOCIATION'. " Default

              " Determine child entity (assume association name matches child entity alias/name, often prefixed with '_')
              lv_potential_child_name = lv_assoc_name.
              IF lv_potential_child_name(1) = '_'.
                lv_potential_child_name = lv_potential_child_name+1.
              ENDIF.

              " Try to find matching entity by clean name or alias
              READ TABLE it_entities INTO ls_found_entity
                WITH KEY entity_name = clean_entity_name( lv_potential_child_name ).
              IF sy-subrc <> 0.
                READ TABLE it_entities INTO ls_found_entity
                  WITH KEY alias = clean_entity_name( lv_potential_child_name ).
              ENDIF.

              IF sy-subrc = 0.
                ls_association-child_entity = ls_found_entity-entity_name.
              ELSE.
                " Fallback if no direct match, e.g., use the cleaned association name
                ls_association-child_entity = clean_entity_name( lv_potential_child_name ).
              ENDIF.


              " Check for COMPOSITION
              IF lv_line_upper CS 'COMPOSITION'.
                ls_association-association_type = 'COMPOSITION'.
              ENDIF.

              " Check if create is allowed
              IF lv_line_upper CS 'CREATE;'.
                ls_association-has_create = abap_true.
              ENDIF.

              " Extract cardinality (e.g., (1), (0..*), (1..*) )
              FIND REGEX '\((0\.\.\*|1\.\.\*|0\.\.1|1)\)' IN lv_line_upper SUBMATCHES lv_cardinality_match.
              IF sy-subrc = 0.
                ls_association-cardinality = lv_cardinality_match.
              ENDIF.

              APPEND ls_association TO rt_associations.
            ENDIF.
          ENDIF.
        ENDIF.

      " End of entity
      ELSEIF lv_line_upper = '}' AND lv_in_entity = abap_true.
        lv_in_entity = abap_false.
        CLEAR lv_current_entity.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_field_mappings.
    DATA: ls_mapping TYPE ty_field_mapping,
          lv_current_entity TYPE string,
          lv_in_entity TYPE abap_bool, " Added to track current entity context
          lv_in_mapping TYPE abap_bool.
    DATA: lv_line TYPE string,
          lv_line_upper TYPE string.
    DATA: lv_define_pos TYPE i,
          lv_remainder TYPE string,
          lv_dummy TYPE string,
          lv_cds_part TYPE string,
          lv_table_part TYPE string.


    CLEAR rt_mappings.

    LOOP AT it_source INTO lv_line.
      lv_line_upper = to_upper( condense( lv_line ) ).

      " Track current entity
      IF lv_line_upper CS 'DEFINE BEHAVIOR FOR'.
        FIND 'DEFINE BEHAVIOR FOR' IN lv_line_upper MATCH OFFSET lv_define_pos.
        IF sy-subrc = 0.
          DATA(lv_start_define_offset) = lv_define_pos + strlen( 'DEFINE BEHAVIOR FOR' ).
          lv_remainder = lv_line_upper+lv_start_define_offset.
          CONDENSE lv_remainder.
          SPLIT lv_remainder AT space INTO lv_current_entity lv_dummy.
          lv_in_entity = abap_true.
        ENDIF.

      " Start of mapping section within an entity
      ELSEIF lv_in_entity = abap_true AND lv_line_upper CS 'MAPPING FOR'.
        lv_in_mapping = abap_true.

      " Parse field mappings when inside mapping section and inside an entity
      ELSEIF lv_in_mapping = abap_true AND lv_in_entity = abap_true AND lv_line_upper CS '='.
        " Parse field mapping like "sales_order_id = sales_order_id;"
        SPLIT lv_line_upper AT '=' INTO lv_cds_part lv_table_part.
        CONDENSE: lv_cds_part, lv_table_part.
        REPLACE ALL OCCURRENCES OF ';' IN lv_table_part WITH ''.

        IF lv_cds_part IS NOT INITIAL AND lv_table_part IS NOT INITIAL.
          ls_mapping-entity_name = lv_current_entity.
          ls_mapping-cds_field = clean_entity_name( lv_cds_part ).
          ls_mapping-table_field = clean_entity_name( lv_table_part ).
          APPEND ls_mapping TO rt_mappings.
        ENDIF.

      " End of mapping section (and potentially entity)
      ELSEIF lv_line_upper = '}'.
        IF lv_in_mapping = abap_true. " Closing mapping section
          lv_in_mapping = abap_false.
        ENDIF.
        IF lv_in_entity = abap_true. " Closing entity section
          lv_in_entity = abap_false.
          CLEAR lv_current_entity.
        ENDIF.
      ENdIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_standard_operations.
    rt_operations = VALUE string_table(
      ( `CREATE` )
      ( `UPDATE` )
      ( `DELETE` )
      ( `READ` )
    ).
  ENDMETHOD.

  METHOD clean_entity_name.
    rv_clean_name = iv_raw_name.
    " Remove any unwanted characters or formatting
    REPLACE ALL OCCURRENCES OF '  ' IN rv_clean_name WITH ' '.
    CONDENSE rv_clean_name.
  ENDMETHOD.

ENDCLASS.
