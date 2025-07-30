behoud_eerste_element_uit_prioriteitslijst_en_ontdubbel_ongeacht_volgorde <- function(lst, prioriteit_vec) {
  aangepaste_lst <- lapply(lst, function(vec) {
    aanwezige <- intersect(prioriteit_vec, vec)
    if (length(aanwezige) > 0) {
      eerste <- aanwezige[1]
      rest <- setdiff(vec, prioriteit_vec)
      unique(c(eerste, rest))
    } else {
      unique(vec)
    }
  })
  
  # Ontdubbel de list waarbij volgorde van elementen niet uitmaakt
  unieke_keys <- sapply(aangepaste_lst, function(x) paste(sort(x), collapse = "\r"))
  aangepaste_lst[!duplicated(unieke_keys)]
}
________________________________________
#🧪 Voorbeeld:
  mijn_list <- list(
    c("a", "b", "c"),
    c("x2", "x1", "y"),
    c("x3", "x1", "x2"),
    c("x2", "x2", "x3"),
    c("x1", "x3", "a"),
    c("x3", "x1")  # zelfde als vorige maar omgedraaid
  )
prioriteit_vec <- c("x1", "x2", "x3")
behoud_eerste_element_uit_prioriteitslijst_en_ontdubbel_ongeacht_volgorde(mijn_list, prioriteit_vec)


f <- get("foo")
environmentName(environment(f))

find("foo")