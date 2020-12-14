#add_granite_application(01_device_creation 01_device_creation.cpp)
#target_compile_definitions(01_device_creation PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")

#add_granite_application(02_object_creation 02_object_creation.cpp)
if (NOT ANDROID)
#    target_compile_definitions(02_object_creation PRIVATE ASSET_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}/assets\")
endif()
