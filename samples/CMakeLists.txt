add_granite_offline_tool(01_device_creation 01_device_creation.cpp)
target_compile_definitions(01_device_creation PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")

add_granite_offline_tool(02_object_creation 02_object_creation.cpp)
if (NOT ANDROID)
    target_compile_definitions(02_object_creation PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")
endif()

add_granite_offline_tool(03_frame_contexts 03_frame_contexts.cpp)
if (NOT ANDROID)
    target_compile_definitions(03_frame_contexts PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")
endif()


add_granite_offline_tool(04_shaders_and_programs 04_shaders_and_programs.cpp)
if (NOT ANDROID)
    target_compile_definitions(04_shaders_and_programs PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")
endif()


add_granite_offline_tool(05_descriptor_sets_and_binding_model 05_descriptor_sets_and_binding_model.cpp)
if (NOT ANDROID)
    target_compile_definitions(05_descriptor_sets_and_binding_model PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")
endif()

#add_granite_application(06_wsi_sdl2 06_wsi_sdl2.cpp)
#target_compile_definitions(06_wsi_sdl2 PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")
