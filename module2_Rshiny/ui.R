library(shiny)
library(ggplot2)
library(shinyWidgets)

ui <- fluidPage(

  helpText("App builder: Jonquil Liao: zliao42@wisc.edu.   ",
           "   To report problems: Contact Yike Wang: wang2557@wisc.edu / Runze You: ryou3@wisc.edu"),
  
  titlePanel(h1("Bodyfat Calculator",align="center")),
  
  setBackgroundImage(
    src = "beauty.jpg"
  ),
  
  
  sidebarPanel(
    
    p(h4("Please fill in the input boxes and click \'Submit\'",align="center")),
    tags$head(
      tags$style("label{font-family: BentonSans Book;}")
    ),
    tags$style(".well {background-color:pink;}"),
    
    selectInput('Gender',  h3("Gender"), c("male","female")),
    sliderInput('Age',  h3("Age (Years)"), min=1, max=100,
                value=23, step=1, round=0),
    numericInput('Chest', label = h3("Chest (cm)"), value = 93.1),
    numericInput('Abdomen', label = h3("Abdomen (cm)"), value = 85.2),
    numericInput('Wrist', label = h3("Wrist circumference (cm)"), value = 17.1),
    numericInput('Weight', label = h3("Weight (kg)"), value = 72.6),
    numericInput('Height', label = h3("Height (cm)"), value = 180),
    
    checkboxInput('Keep', 'Keep my information secret'),
    checkboxInput('Notkeep', 'I would like to let my information out for research purposes'),
    
    submitButton("Submit")
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("How to use the Body fat Calculator", br(), 
               
               p(h4("When to Take Measurements"),"To get the most accurate result, you'll want to take measurements first thing in the morning. Your weight can fluctuate throughout the day so it's smart to get your numbers before you've had anything to eat or drink."),
               p(h4("What to Use"),"Once you've weighed yourself, you'll need to take body measurements. Use a soft, flexible (cloth or fiberglass) tape measure to gather the numbers. Do not use a tape measure that is elastic."),
               p(h4("How to Take Your Measurements"),"When you measure the circumference of each body part, the tape should feel firm enough against your skin so that it stays in place but not so tight that it causes an indentation."),
               p(h4("Bioelectrical Impedance (BIA)"),"Home scales, gym-quality scales, and devices in your healthcare provider's office may use bioelectrical impedance to estimate body fat. Simply put, the device sends a harmless, completely pain-free electrical signal through your body to measure fat.
You get a quick and instantaneous body fat percentage result. While this method is convenient, certain factors such as hydration level can influence the accuracy of the result."),
               p(h4("Hydrostatic Weighing"),"This submersion method has long been recognized as the most accurate method of measuring body fat percentage. However, it requires a person to be fully immersed in a tank of water while holding their breath.

For many, the endurance required for hydrostatic weighing can be a challenge. It can also be difficult to find a location that performs the test.

There are other methods of measuring body fat not listed here. Whichever method you choose, remember that if you plan to measure your body fat regularly, you need to use the same method each time to get the most accurate data."),
               p(h4("How to Reduce Body Fat"),"Now that you know your body fat percentage, are you inspired to reduce your numbers? The method is a simple equation: burn more calories than you eat.

While the equation is simple, that doesn't mean that the process is easy. Reducing body fat means making changes in all areas of your life, not just in the way you eat and exercise.

Reducing body fat should not be a goal for everyone. If you fall into the essential fat or even the athlete category, you may need to consider gaining weight. Speak with your healthcare professional before making a decision.

It's also important to know that these numbers don't apply to pregnant women — you should not try to lose weight during pregnancy.

You'll also want to talk to your healthcare provider if you are trying to lose weight while on medications or during cancer treatment to make sure that it is safe to do so.

If your doctor determines that reducing your body fat percentage is a safe and beneficial health goal, here are some areas for you to consider making changes.", h4("Your Diet"),"How much you eat and what you eat is a huge factor in losing or gaining body fat. You might be tempted to try a popular diet, but fad diets generally don't work. What does work is making small changes, such as:

Reducing your portion sizes.
Eating smaller meals more frequently throughout the day and avoiding the urge to skip breakfast.
Ensuring your diet contains lots of fruits and vegetables to fill you up and give you the nutrients you need.
Adding more fiber to your diet, which fills you up and makes it less likely you'll reach for less healthy snacks throughout the day.
Avoid sugary drinks and junk food.
Limit how much alcohol you drink.")
),
      
      tabPanel("Bodyfat related information",br(), img(src = "bodyfatjpg.png", height = 500, width = 700),br(), p(h4("The scientific term for body fat is \"adipose tissue.\" Adipose tissue serves a number of important functions.    Its primary purpose is to store lipids from which the body creates energy.     In addition, it secretes a number of important hormones, and provides the body with some cushioning as well as insulation.

Body fat includes essential body fat and storage body fat.      Essential body fat is a base level of fat that is found in most parts of the body.      It is necessary fat that maintains life and reproductive functions.        The amount of essential fat differs between men and women, and is typically around 2-5% in men, and 10-13% in women.       The healthy range of body fat for men is typically defined as 8-19%, while the healthy range for women is 21-33%. While having excess body fat can have many detrimental effects on a person's health, insufficient body fat can have negative health effects of its own, and maintaining a body fat percentage below, or even at the essential body fat percentage range is a topic that should be discussed with a medical professional.

    Storage fat is fat that accumulates in adipose tissue, be it subcutaneous fat (deep under the dermis and wrapped around vital organs) or visceral fat (fat located inside the abdominal cavity, between organs), and references to body fat typically refer to this type of fat. While some storage fat is ideal, excess amounts of storage fat can have serious negative health implications.

    Excess body fat leads to the condition of being overweight and eventually to obesity given that insufficient measures are taken to curb increasing body fat. Note that being overweight does not necessarily indicate an excess of body fat. A person's body weight is comprised of multiple factors including (but not limited to) body fat, muscle, bone density, and water content. Thus, highly muscular people are often classified as overweight.

    The rate at which body fat accumulates is different from person to person and is dependent on many factors including genetic factors as well as behavioral factors such as lack of exercise and excessive food intake. Due to varying factors, it can be more difficult for certain people to reduce body fat stored in the abdominal region. However, managing diet and exercise has been shown to reduce stored fat. Note that both women and men store body fat differently and that this can change over time. After the age of 40 (or after menopause in some cases for women), reduced sexual hormones can lead to excess body fat around the stomach in men, or around the buttocks and thighs of women.")),
              ), 
      tabPanel("Your Bodyfat calculation result" ,br(),br(),
               p(h4("Your body fat percentage is:")),
               
               conditionalPanel(
                 condition ="output.value <= 0",
                 p(h3("Oops, seems your data has some abnormal problems."))
               ),
               
               conditionalPanel(
                 condition = "output.value > 0 ",
                 verbatimTextOutput("txtOutput")
               ),
               br(),p(h4("Normally, people's bodyfat level is as bellow distributed")),plotOutput("plot"),
               
               conditionalPanel(
                 condition = "output.value >40 ",
                 br(),
                 p(h3("Hey, bro!  It seems you need more work out! Check the advices below: ")),
                 br(),
                 p(h4("1. Exercise Daily"),
"Exercise daily for at least an hour. You do not have to kill yourself from running, jogging, etc., but you should have some sort of moderate physical activity in your everyday life. If you're looking to shed a few pounds fast, do a higher-level intensity workout. For example, go on a walk at a brisk pace for an hour. Or, you can jog and set certain intervals to sprint during that hour. Make sure you're not in severe pain during your workout. Just a warning, your muscles will ache after a high intensity workout. It may be irritating, but that means your body is changing for the better. Be sure to stay hydrated, stretch, and eat foods with a decent amount of protein after each workout. The protein will help keep your muscles, not fat, rebuilding."),
p(h4("2.Eat the Right Foods and Portion Each Meal"),"No matter how bad your stomach is telling you to go for candy over healthy food, try to stay away from sweets. Sugar from candy will not help you get in shape. Even if it's just a single candy bar, one will eventually lead to another. Fruits and vegetables are the best thing to eat when getting into shape. Apples, for example, do a good job at making the stomach feel full for up to 3 to 4 hours. Green vegetables such as green beans and broccoli keep the digestive system clean and running."),
p(h4("3.Keep Track of Calories and Food Intake Per Day"),"Keeping track of how many calories you eat in a day will be helpful in planning out your physical exercising. Ever wonder why body builders' body masses are so big? That's because they plan out their meals and take in more (healthy) calories than the average person. On the other hand, losing weight and striving for a skinnier physique will involve more physical exercise than calories you ingest."),
p(h4("4.Be Sure to Get Sleep"),"Even though most of us have eight-hour jobs during the day or night, it is crucial to get enough sleep to recharge the body's batteries. Six to eight hours of sleep will keep the body going throughout the day, but if you happen to feel tired at any point after coming home from work, by all means take a small nap before exercising. You should only nap for about a half hour. This will prevent you from staying up later in the night.")
),

conditionalPanel(
  condition = "output.value <= 40",
  br(),
  p(h3("Wow! You look quite healthy by now! ")),
  img(src = "god.jpg", height = 300, width = 600)
)
               )
               
               )
      ),
  

    )





