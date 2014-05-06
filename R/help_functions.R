#' @import httr
#' @import RJSONIO
#' @import yaml
#' @import XML
#' @import RCurl
#' 
##### HELP FUNCTIONS ##### 
datacamp_logged_in = function() {
  if (exists(".DATACAMP_ENV")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

upload_chapter_json = function(theJSON, file_name, open = TRUE) {
  base_url = paste0(.DATACAMP_ENV$base_url, "/chapters/create_from_r.json")
  auth_token = .DATACAMP_ENV$auth_token
  url = paste0(base_url,"?auth_token=", auth_token)
  x = try(POST(url = url, body = theJSON, add_headers(c(`Content-Type` = "application/json", `Expect` = ""))))
  
  if ( class(x) != "response" ) {
    stop("Something went wrong. We didn't get a valid response from the datacamp server. Try again or contact info@datacamp.com in case you keep experiencing this problem.")
  } else { 
    if (is.list(content(x)) ) { 
      if ("course" %in% names(content(x)) ) {  
        course = content(x)$course
        chapter = content(x)$chapter
        new = content(x)$created
        message(paste0("Changes made to course (id:",course$id,"): \"", course$title,"\":"))
        if (new == TRUE) {
          message(paste0("\tCreated new chapter (id:", chapter$id,"): \"", chapter$title,"\".")) 
        } else {
          message(paste0("\tExisting chapter (id:",chapter$id,"): \"", chapter$title,"\" was updated."))
        }
        add_chapter_to_course_yml(file_name, as.integer(chapter$id))
        if (open) {
          browseURL(paste0(.DATACAMP_ENV$redirect_base_url, "/", course$id))
        } 
      } 
      if ("message" %in% names(content(x))) {
        message(content(x)$message)
      }
      if ( "error" %in% names(content(x)) ) {
        message(paste0("Something went wrong:\n", content(x)$error ))
      } 
    } else {
      message(paste0("Something went wrong. Please check whether your course was correctly uploaded to datacamp.com."))
    } 
  } 
}

upload_course_json = function(theJSON, open = TRUE) { 
  base_url = paste0(.DATACAMP_ENV$base_url, "/courses/create_from_r.json")
  auth_token = .DATACAMP_ENV$auth_token
  url = paste0(base_url,"?auth_token=", auth_token)
  x = try(POST(url = url, body = theJSON, add_headers(c(`Content-Type` = "application/json", `Expect` = ""))))
  if ( class(x) != "response" ) {
    stop("Something went wrong. We didn't get a valid response from the datacamp server. Try again or contact info@datacamp.com in case you keep experiencing this problem.")
  } else {
    if (is.list(content(x)) ) {
      if ("course" %in% names(content(x))) {
        course = content(x)$course
        new = content(x)$created
        if (new == TRUE) {
          message(paste0("A new course was created with id ", course$id," and title \"", course$title,"\".")) 
        } else {
          message(paste0("Existing course (id:", course$id,"): \"", course$title,"\" was updated."))
        }
        add_id_to_course_yml(course$id) # write id to course.yml file if it's not already there
        if (open) { 
          browseURL(paste0(.DATACAMP_ENV$redirect_base_url, "/", course$id))
        }
        if ("message" %in% names(content(x))) {
          message(content(x)$message)
        }
      } else if ( "error" %in% names(content(x)) ) {
        message(paste0("Something went wrong:\n", content(x)$error ))
      }
    } else {
      message(paste0("Something went wrong. Please check whether your course was correctly uploaded to DataCamp."))
    }
  }
}

add_id_to_course_yml = function(course_id) {
  yaml_list = load_course_yaml()
  if (is.null(yaml_list$id)) {
    # Add id to the front of the list. As.integer because could be num:
    yaml_list = append(yaml_list, list(id = as.integer(course_id)), after = 0)
    
    yaml_output = as.yaml(yaml_list,line.sep="\n")
    write(yaml_output, file="course.yml")
    message("The id was added to your course.yml file.")
  } else if (yaml_list$id != course_id) {
    stop(paste0("Something strange happened. Your course.yml file has course id ", course$id, ", whereas the server just returned ", course_id, " . Please compare your course.yml file with the web interface."))
  }
}

get_chapter_id = function(file_name) {
  course = load_course_yaml()
  chapter_index = which(sapply(course$chapters, function(x) {names(x)}) == file_name)
  return(as.integer(chapter_index))
}

add_chapter_to_course_yml = function(chapter_file_name, chapter_id) {
  chapter_index = get_chapter_id(chapter_file_name)
  if (length(chapter_index) == 0) {
    yaml_list = load_course_yaml()
    
    n = length(yaml_list$chapters)
    yaml_list$chapters[[n+1]] = structure(list(chapter_id), names = chapter_file_name)

    yaml_output = as.yaml(yaml_list,line.sep="\n")
    write(yaml_output, file="course.yml")
    message("The chapter was added to your course.yml file.")
  }
}

render_chapter_json_for_datacamp = function(file_name, payload, force) {
  # Extract basic course info:
  course = load_course_yaml()
  if (is.null(course$id)) {
    stop("Error: course.yml does not contain a course id. Please upload your course before uploading chapters.")
  }
  output_list = list(force = force,
                    course = course$id,
                    email  = .DATACAMP_ENV$email,
                    chapter=list(
                      title_meta=payload$title_meta,
                      title=payload$title,
                      description=payload$description
                    ) 
  )
  
  # Extract chapter id and index from course.yml. If found, add to outputList
  course = load_course_yaml()
  chapter_index = get_chapter_id(file_name)
  if (length(chapter_index) != 0) { # existing chapter, add info to output_list
    output_list$chapter$id = as.integer(course$chapters[[chapter_index]])
    output_list$chapter$number = chapter_index
  }
  
  # Extract for each exercise the relevant information:
  slides = payload$slides 
  exerciseList = list() 
  for(i in 1:length(slides)) {
    slide = slides[[i]]
    if ( !is.null(slide$type) && slide$type == "VideoExercise" ) {
      exerciseList[[i]] = list(  title         = html2txt(slide$title),
                                 assignment    = clean_up_html(slide$content), 
                                 number        = slide$num,
                                 video_link    = gsub("[\r\n]", "", extract_code(slide$video_link$content)) )
      exerciseList[[i]][["type"]] = slide$type
    } else {
      exerciseList[[i]] = list(  title         = html2txt(slide$title),
                                 assignment    = clean_up_html(slide$content), 
                                 number        = slide$num,
                                 instructions  = clean_up_html(slide$instructions$content), 
                                 hint          = clean_up_html(slide$hint$content),
                                 sample_code   = extract_code( slide$sample_code$content ),
                                 solution      = extract_code( slide$solution$content ),
                                 sct           = extract_code( slide$sct$content ),
                                 pre_exercise_code = extract_code( slide$pre_exercise_code$content) )
      if (!is.null(slide$type)) {  
        exerciseList[[i]][["type"]] = slide$type
        if (slide$type == "MultipleChoiceExercise") {
          exerciseList[[i]][["instructions"]] = make_multiple_choice_vector(exerciseList[[i]][["instructions"]])
          if (!is.null(slide$contains_graph)) {
            exerciseList[[i]][["contains_graph"]] = slide$contains_graph
          }
        }
      }      
    }
  }
  
  # Join everything: 
  output_list$chapter$exercises = exerciseList 
  
  # Make JSON: 
  toJSON(output_list) 
}

extract_code = function(html) {
  if (!is.null(html)) {
    if (nchar(html)!=0) {
      r = regexpr("<code class=\"r\">(.*?)</code>",html)
      code = regmatches(html,r)
      code = gsub("<code class=\"r\">|</code>","",code)
      code = html2txt(code)
      
      # solve bug: when quotes are within quotes, we need different type of quotes! e.g. "c('f','t','w')"
      code = gsub("[\\]\"","'",as.character(code))
      
      return(code)
    }}
} 

# Convenience function to convert html codes:
html2txt <- function(str) {
  str = paste0("<code>",str,"</code>")
  xpathApply(htmlParse(str, asText=TRUE),"//body//text()", xmlValue)[[1]]
}

# Remove paragraphs:
clean_up_html = function(html) {
  #   html = gsub("<p>|</p>","",html)
  return(html)
}

# Function to create an array with the multiple choice options: 
make_multiple_choice_vector = function(instructions) { 
  pattern = "<li>(.*?)</li>"
  instruction_lines =  strsplit(instructions,"\n")[[1]]
  r = regexec(pattern, instruction_lines)
  matches = regmatches(instruction_lines, r)
  extracted_matches = sapply(matches, function(x) x[2])
  multiple_choice_vector = extracted_matches[!is.na(extracted_matches)]
  
  return(multiple_choice_vector)
}

# Load yaml:
load_course_yaml = function() { 
  # Step 1: Load the yaml file such that we have a list with all course information:
  if (!file.exists("course.yml")) { 
    stop("Seems like there is no course.yml file in the current directory.") 
  }
  course = try(suppressWarnings(yaml.load_file("course.yml")))
  if (inherits(course,"try-error")) {
    stop(paste("There's a problem loading your course.yml file. Please check the documentation to find out what the course.yml file should contain. Go to:",doc_url()))
  }

  # Step 2: Check the course yaml object on inconsistencies. 
  # This is necessary since the rest of the code assumes a certain structure. 
  # Everything should be clean before it's uploaded to the back-end or further processed in R.
  check_course_object(course)

  # Only relevant when uploading (TODO?)
#   if (is.null(course$id)) {
#     sure = readline("No id found in course.yml. This will create a new course, are you sure you want to continue? (Y/N) ")
#     if (!(sure == "y" || sure == "Y" || sure == "yes" || sure == "Yes")) { return(message("Aborted.")) }
#   }

  return(course)
}

# Check the course yaml object
check_course_object = function(course) {
  ## STEP 1: Check the course info
  # All basic info present in yaml?
  min_names = c("title", "author_field", "description")
  present_names = min_names %in% names(course)
  if (!all(present_names)) {
    stop(paste("Looks like your course.yml file is missing the information:", 
               paste(min_names[!present_names], collapse=" and "), 
               "\nHave a look at the documentation on:", doc_url(),"."))
  }
  # Any empty elements
  min_course = course[min_names[present_names]]
  empty = sapply(min_course, function(x){
   is.null(x) || (x == "") || (x == " ") || is.na(x)
  })
  if (any(empty)) {
    stop(paste("Looks like your course.yml file is missing information for the field(s): ", 
               paste(names(min_course[empty]), collapse=" and "), 
               ".\nThese cannot be empty. Please add that in your course.yml file.\nHave a look at the documentation on: ", doc_url(),".",sep=""))
  }
  
  ## STEP 2: Check the chapters
  check_chapters(course)
  
  invisible()
}

# Checks on chapter part:
check_chapters = function(course) {
  chapters = course[["chapters"]]
  if (!is.null(chapters)) {
    chapters = unlist(chapters)
    if (!is.vector(chapters)) {
      stop(paste("Something is wrong with the chapters section in your course.yml file.\nPlease check the documentation at: ",
                 doc_url(),".", sep=""))
    }
    
    # Are the chapter ids unique and do they exist?
    empty_chapters = sapply(chapters, function(x){
      is.null(x) || (x == "") || (x == " ") || is.na(x)
    })
    if (any(empty_chapters) || (length(course$chapters)!=length(chapters))) {
      stop(paste("Something is wrong with the chapters section in your course.yml file.\nYour chapter id can't be empty.\nPlease check the documentation at: ",
                 doc_url(), ".", sep=""))
    }
    
    if (length(chapters) != length(unique(chapters))) {
      stop(paste("Something is wrong with the chapters section in your course.yml file.\nYour chapter ids should be unique.\nPlease check the documentation at: ",
                 doc_url(), ".", sep=""))
    }
    # Are the Rmd files unique and do they exist?
    if (length(names(chapters)) != length(unique(names(chapters)))) {
      stop(paste("Something is wrong with the chapters section in your course.yml file.\nYour chapter file names should be unique.\nPlease check the documentation at: ",
                 doc_url(), ".", sep=""))
    }
    existing_files = file.exists(names(chapters))
    if (!all(all(existing_files))) {
      stop(paste("Something is wrong with the chapters section in your course.yml file.\nThe following files are specified in the course.yml but do not exist in your working directory: ",
                 names(chapters)[!existing_files],".", sep=""))
    }    
  }
}

# Documentation path
doc_url = function() { return("http://github.com/Data-Camp/datacamp") }

# rmd checker
is_rmd = function(text) {
  ex = substr(text, nchar(text)-3, nchar(text))
  is_rmd = (ex == ".Rmd") || (ex == ".RMD") || (ex == ".rmd") || (ex == ".RMd")  
  return(is_rmd)
}