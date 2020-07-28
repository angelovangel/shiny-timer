# a shiny timer app based on shiny mobile
# kind of re-create the apple timer in framework7 (shinyMobile)

# button logic
# start and pause are two buttons, which get shown or hidden depending on the state
# cancel resets everything

library("shinyMobile")
library(dplyr)
library(lubridate)
library(shinyjs)

ui <- f7Page(
	shinyjs::useShinyjs(),
	inlineCSS(list(.orange = "background: orange")), # used to toggle start/stop buttons color
	
	includeScript(path = "js/NoSleep.min.js"),
	includeScript(path = "js/nosleep.js"),
	
	init = f7Init(skin = "ios", theme = "dark"),
	title = "Shiny Countdown", 
	f7SingleLayout(
		navbar = f7Navbar(
			title = "Shiny countdown timer", hairline = TRUE, shadow = FALSE
			),
		f7Flex(
			# https://framework7.io/docs/typography.html#flexbox
			class = "flex-direction-column justify-content-center align-items-center",
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
							borderWidth = 20),
			tags$hr(),
			h4("Minutes"),
			f7Stepper(inputId = "selectMinutes", 
								label = "", min = 1, max = 59, value = 5, 
								size = "large", wraps = TRUE, raised = TRUE, fill = TRUE, color = "orange"),
			h4("Hours"),
			f7Stepper(inputId = "selectHours", 
								label = "", min = 0, max = 24, value = 0, 
								size = "large", wraps = TRUE, raised = TRUE, fill = TRUE, color = "orange"),
		),
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
	
	observeEvent(input$selectMinutes, {
		SecsFromMinutes <- as.numeric(minutes(input$selectMinutes)) # as.numeric(minutes(5)) returns 300
		SecsFromHours <- as.numeric(hours(input$selectHours))
		# set timer to seconds selected
		timer(SecsFromMinutes + SecsFromHours) 
		
		updateF7Gauge(session, id = "gauge", 
									#value = (timer()/input$selectMinutes/60)*100, 
									labelText = paste(seconds_to_period( timer() )) # this automatically prints pretty times
									)
	})
	
	# separate observer to hours
	observeEvent(input$selectHours, {
		SecsFromMinutes <- as.numeric(minutes(input$selectMinutes)) # as.numeric(minutes(5)) returns 300
		SecsFromHours <- as.numeric(hours(input$selectHours))
		# set timer to seconds selected
		timer(SecsFromMinutes + SecsFromHours) 
		
		updateF7Gauge(session, id = "gauge", 
									#value = (timer()/input$selectMinutes/60)*100, 
									labelText = paste(seconds_to_period( timer() ))
		)
	})
	# observer to manage active or inactive state of timer
	observe({
		invalidateLater(1000, session)
		isolate({
			if(active() )
			{
				timer(timer() - 1)
				updateF7Gauge(session, 
											id = "gauge", 
											value = (timer()/input$selectMinutes/60)*100, 
											labelText = paste(seconds_to_period( timer() )) )
				if(timer() < 1)
				{
					active(FALSE)
					f7Notif(text = "Time is up!", 
									icon = f7Icon("alarm"), 
									session = session)
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
		shinyjs::enable("selectHours")
		
		SecsFromMinutes <- as.numeric(minutes(input$selectMinutes)) # as.numeric(minutes(5)) returns 300
		SecsFromHours <- as.numeric(hours(input$selectHours))
		timer(SecsFromMinutes + SecsFromHours)
		# reset gauge also
		updateF7Gauge(session, 
									id = "gauge", 
									value = 100, 
									labelText = paste(seconds_to_period( timer() )) )
	})
	
}

shiny::shinyApp(ui, server)