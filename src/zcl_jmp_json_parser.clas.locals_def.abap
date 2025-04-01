*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature
types: begin of zut_data,
  type type c length 1,
  data type ref to data,
  json type REF TO ZIF_JMP_JSON,
end of zut_data.
types: zut_array_tab
  type standard table of zut_data.
types: begin of zut_hash_element,
  key type string.
  include type zut_data as value.
types: end of zut_hash_element.
types: zut_hash_tab type hashed table of zut_hash_element
  with unique key key.
