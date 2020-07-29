# a shiny timer app based on shiny mobile
# kind of re-create the apple timer in framework7 (shinyMobile)

# button logic
# start and pause are two buttons, which get shown or hidden depending on the state
# cancel resets everything

library("shinyMobile")
library(dplyr)
library(lubridate)
library(shinyjs)
library(V8) #------------ needed for extendShinyjs()

# code to call three times the sound alert function
	
	jscode <- "
		shinyjs.alert = function(params) {
		playAlert(params)
		}"


ui <- f7Page(
	shinyjs::useShinyjs(),
	
	inlineCSS(list(.orange = "background: orange")), # used to toggle start/stop buttons color
	
	includeScript(path = "js/alert.js"),
	includeScript(path = "js/NoSleep.min.js"),
	includeScript(path = "js/nosleep.js"),
	
	extendShinyjs(text = jscode),
	
	init = f7Init(skin = "ios", theme = "dark"),
	title = "Shiny timer", 
	f7SingleLayout(
		navbar = f7Navbar(
			title = "Shiny timer", hairline = TRUE, shadow = FALSE
			),
		f7Flex(
			# https://framework7.io/docs/typography.html#flexbox
			class = "flex-direction-column justify-content-center align-items-stretch",
			#class = "flex-direction-column",
			tags$hr(),
			f7Gauge("gauge", 
							value = 100, 
							valueFontSize = "0",
							type = "circle",
							size = 250, 
							labelText = 100, 
							labelFontSize = "34", 
							labelTextColor = "orange",
							borderColor = "orange", 
							borderBgColor = "gray", 
							borderWidth = 5),
			tags$hr(),
			#h4("Minutes"),
			f7Flex(
				#class ="justify-content-space-between align-content-space-between",
				f7Slider("selectMinutes", min = 0, max = 9, value = 5, step = 1,
								 color = "orange", label = "min 1x", vertical = T),
				f7Slider("selectMinutes10x", min = 0, max = 50, value = 0, step = 10, 
								 color = "orange", label = "min 10x", vertical = T),
				f7Slider("selectHours", min = 0, max = 9, value = 0, step = 1,
								 color = "orange", label = "hours", vertical = T),
			)
			),
			
		# 	f7Stepper(inputId = "selectMinutes", 
		# 						label = "", min = 1, max = 59, value = 5, 
		# 						size = "large", wraps = TRUE, raised = TRUE, fill = TRUE, color = "orange"),
		# 	h4("Hours"),
		# 	f7Stepper(inputId = "selectHours", 
		# 						label = "", min = 0, max = 24, value = 0, 
		# 						size = "large", wraps = TRUE, raised = TRUE, fill = TRUE, color = "orange"),
		# ),
		toolbar = f7Toolbar(f7Button(inputId = "start", label = "start", color = "green", rounded = FALSE, size = "large"),
												f7Button(inputId = "pause", label = "pause", color = "gray", rounded = FALSE, size = "large"),
												f7Button(inputId = "cancel", label = "cancel", color = "red", rounded = FALSE, size = "large"),
												position = "bottom", hairline = TRUE, shadow = FALSE, icons = TRUE)
	)
)


server <- function(input, output, session){
	
	# at start hide pause
	shinyjs::toggle("pause")
	
	# initialize 2 reactive vals
	# 
	timer <- reactiveVal(100) # set to 100 independent of select time
	active <- reactiveVal(FALSE)
	SecsFromSelector <- reactiveVal(0)
	
	observeEvent(input$selectMinutes, {
		SecsFromSelector(as.numeric(minutes(input$selectMinutes)) +
											as.numeric(minutes(input$selectMinutes10x)) +
											as.numeric(hours(input$selectHours))
										 )
		
		# set timer to seconds selected
		timer( SecsFromSelector() ) 
		
		updateF7Gauge(session, id = "gauge", 
									#value = (timer()/input$selectMinutes/60)*100, 
									labelText = paste(seconds_to_period( timer() )) # this automatically prints pretty times
									)
	})
	
	observeEvent(input$selectMinutes10x, {
		SecsFromSelector(as.numeric(minutes(input$selectMinutes)) +
										 	as.numeric(minutes(input$selectMinutes10x)) +
										 	as.numeric(hours(input$selectHours))
		)
		
		# set timer to seconds selected
		timer( SecsFromSelector() ) 
		
		updateF7Gauge(session, id = "gauge", 
									#value = (timer()/input$selectMinutes/60)*100, 
									labelText = paste(seconds_to_period( timer() )) # this automatically prints pretty times
		)
	})
	
	# separate observer to hours
	observeEvent(input$selectHours, {
		SecsFromSelector(as.numeric(minutes(input$selectMinutes)) +
										 	as.numeric(minutes(input$selectMinutes10x)) +
										 	as.numeric(hours(input$selectHours))
		)
		
		# set timer to seconds selected
		timer( SecsFromSelector() ) 
		
		updateF7Gauge(session, id = "gauge", 
									#value = (timer()/input$selectMinutes/60)*100, 
									labelText = paste(seconds_to_period( timer() ))
		)
	})
	# observer to manage active or inactive state of timer
	observe({
		invalidateLater(100, session)
		isolate({
			if(active() )
			{
				timer(timer() - 0.1)
				#cat(timer(), "\n")
				updateF7Gauge(session, 
											id = "gauge", 
											value = (timer() / SecsFromSelector() ) * 100,
											labelText = paste( round(seconds_to_period( timer() ), 0) )
											)
				if(timer() == 0)
				{
					active(FALSE)
					
					#timer(0) # brute force set to 0 
					updateF7Gauge(session, 
												id = "gauge", 
												value = (timer() / SecsFromSelector() ) * 100,
												labelText = paste( round(seconds_to_period( timer() ), 0) )
					)
					f7Dialog(text = "Time is up!", 
									type = "alert",
									session = session)
					# try to play sound with alert.js
					for (i in 1:5) {
						Sys.sleep(1); js$alert('purr')
					}
					#cat(timer())
				}
			}
		})
	})
	
	# observers for buttons
	# button states are also managed here
	observeEvent(input$start, {
		active(TRUE)
		shinyjs::toggle("start") # hide start
		shinyjs::toggle("pause") # show pause
		shinyjs::disable("selectMinutes")
		shinyjs::disable("selectMinutes10x")
		shinyjs::disable("selectHours")
	})
	
	observeEvent(input$pause, {
		active(FALSE)
		shinyjs::toggle("start") # show start
		shinyjs::toggle("pause") # hide pause
		shinyjs::html("start", html = "continue")
		shinyjs::addCssClass("start", class = "orange")
		#shinyjs::enable("selectMinutes")
		#shinyjs::enable("selectHours")
	})
	
	observeEvent(input$cancel, {
		active(FALSE)
		# reset start/pause to initial state
		shinyjs::show("start")
		shinyjs::removeCssClass("start", class = "orange")
		shinyjs::html("start", "start")
		shinyjs::hide("pause")
		
		shinyjs::enable("selectMinutes")
		shinyjs::enable("selectMinutes10x")
		shinyjs::enable("selectHours")
		
		SecsFromSelector(as.numeric(minutes(input$selectMinutes)) +
										 	as.numeric(minutes(input$selectMinutes10x)) +
										 	as.numeric(hours(input$selectHours))
		)
		
		timer( SecsFromSelector() )
		
		# reset gauge also
		updateF7Gauge(session, 
									id = "gauge", 
									value = 100, 
									labelText = paste(seconds_to_period( timer() )) )
	})
	
	# session$onSessionEnded(stopApp)
}

shiny::shinyApp(ui, server)