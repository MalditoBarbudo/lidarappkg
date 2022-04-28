app <- ShinyDriver$new("../../")
app$snapshotInit("init_test_den")

app$setInputs(`mod_dataInput-lidar_var_sel` = "DEN")
app$snapshot()
