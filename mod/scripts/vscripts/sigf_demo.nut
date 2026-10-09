// Skate 3 Takeover: the recorded demo. The bot skates a lap of the park while the captions say what is happening.
::SkDemoState <- { started = false }

::SkDemoStart <- function() {
	if (::SkDemoState.started) return
	::SkDemoState.started = true
	local h = SigfHost()
	h.SetOrigin(Vector(-192.0, -1380.0, ::SK_FZ + 2.0))
	h.SetVelocity(Vector(0.0, 0.0, 0.0))
	::Sk.auto = true
	::Sk.route = ::SkRoute
	::Sk.wi = 1
	::Sk.hdg = 90.0
	::Sk.dirx = 0.0
	::Sk.diry = 1.0
	::Sk.speed = 300.0
	::Sk.score = 0
	::Sk.combo = 1
	::Sk.arm = false
	::Sk.air = false
	::Sk.lastPos = null
}

::SkDemoNote <- function(text, sec = 3.5) {
	SigfText(text, -1.0, 0.8, sec, "255 255 255", 3)
}

SigfDemo(0.2, function() { ::SkIntro() })
SigfDemo(2.6, function() { ::SkDemoStart() })
SigfDemo(3.3, function() { ::SkDemoNote("Wheatley and a turret are shredding the chamber", 2.4) })
SigfDemo(5.6, function() { ::SkDemoNote("Pop off the pyramid: air time scores tricks", 2.6) })
SigfDemo(8.4, function() { ::SkDemoNote("Ride into the blue portal...", 2.2) })
SigfDemo(10.6, function() { ::SkDemoNote("...and exit the orange one straight into a kicker", 3.2) })
SigfDemo(14.0, function() { ::SkDemoNote("Grind the rail, stack the combo", 3.0) })
SigfDemo(19.2, function() { ::SkDemoNote("Bad landing? Welcome to the Hall of Meat", 3.0) })
