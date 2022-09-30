
set(MY_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}) 
macro(json2hpp target name input_file output_file)
	ADD_CUSTOM_COMMAND(TARGET ${target} PRE_BUILD
		COMMAND "./json2hpp.exe" ${name} ${input_file} ${output_file}
		WORKING_DIRECTORY ${MY_DIRECTORY}
	)
endmacro(json2hpp)