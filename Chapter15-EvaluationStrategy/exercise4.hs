-- Just want to give a shoutout to Julien Rathke - that fucking cunt - who plagiarised this question for his shitty Prog III exam.
fibs :: [Integer]
fibs = 0:1:[x + y | (x, y) <- zip (tail fibs) fibs]
