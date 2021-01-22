survey_skeleton<-function(df){df %>%
    group_by(question_group,
             research_question,
             indicator_english,
             question) %>%
    summarise(
      name= unique(q_code),
      `hint::english`= unique(hint_english),
      required= unique(required),
      relevant=unique(skip_logic),
      type= paste(unique(question_type),name,collapse = " "),
      choice_filter=NA,
      appearance=NA,
      constraint= unique(constraint),
      constraint_message= NA,
      repeat_count= NA ,
      calculation = NA,
    ) %>% ungroup() %>%
    select(-question_group, - research_question,-indicator_english) %>%
    select( type,name,label=question, everything())
}


choices_skeleton<-function(df){
  df %>%
    group_by(q_code) %>%
    mutate(name=str_replace_all(choice,c("'"="")),
           name= snakecase::to_snake_case(name)) %>%
    ungroup() %>%
    select(list_name=q_code,
           name,
           label=choice)

}
