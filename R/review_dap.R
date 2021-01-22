

review_dap<-function(df,file_path){
  max_choice_char<-df %>%
    transmute(nchar(choice)) %>% max()
  template_converted<-df %>%
    group_by(question_group,
             research_question,
             indicator_english,
             question) %>%
    summarise(choice=paste0(choice, collapse=";\n"),
              constraint= unique(constraint),
              hint_english= unique(hint_english),
              skip_logic=unique(skip_logic)
    )
  wb <- createWorkbook()
  addWorksheet(wb, "review_DAP")
  writeData(wb, "review_DAP", x = template_converted)
  # to make sure your text shows as two lines
  setRowHeights(wb, "Sheet 1", rows = 1:nrow(template_converted), height = 30)
  addStyle(wb, sheet = 1, style = createStyle(wrapText = TRUE), rows = 1:nrow(template_converted)+1, cols = 5)

  #narrow width produces multiple lines as a consequence of text wrap
  setColWidths(wb, sheet = 1, cols = 5, widths = max_choice_char)
  openxlsx::saveWorkbook(wb,file =file_path, overwrite=T)


}
