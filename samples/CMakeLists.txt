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

add_granite_application(07_linear_allocators 07_linear_allocators.cpp)
target_compile_definitions(07_linear_allocators PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")

add_granite_application(08_render_passes 08_render_passes.cpp)
target_compile_definitions(08_render_passes PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")

add_granite_application(09_synchronization 09_synchronization.cpp)
target_compile_definitions(09_synchronization PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")

add_granite_application(10_pipelines 10_pipelines.cpp)
target_compile_definitions(10_pipelines PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")
