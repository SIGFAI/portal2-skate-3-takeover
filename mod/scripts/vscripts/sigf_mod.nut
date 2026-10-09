// Skate 3 Takeover
IncludeScript("sigf_spawn")
IncludeScript("sigf_park")
IncludeScript("sigf_skate")
IncludeScript("sigf_skaters")
SigfCmd("gameinstructor_enable 0; sv_gameinstructor_disable 1")
::SkMusicAt <- -100.0
::SkMusic <- function() {
	if (Time() - ::SkMusicAt < 30.5) return
	::SkMusicAt = Time()
	SigfSound("sigf/music.wav")
}
::SkIntro <- function() {
	::SkMusic()
	SigfCmd("mat_wireframe 1")
	SigfIn(0.4, function() { SigfCmd("mat_wireframe 0") })
	SigfIn(0.6, function() { SigfCmd("mat_wireframe 1") })
	SigfIn(1.0, function() { SigfCmd("mat_wireframe 0") })
	SigfIn(1.2, function() { SigfCmd("mat_wireframe 1") })
	SigfIn(1.5, function() { SigfCmd("mat_wireframe 0") })
	::SkOv("ov_title", 3.0)
}
SigfAfter(0.5, function() { ::SkStartFast() })
SigfAfter(1.0, function() { ::SkBuildPark() })
SigfEvery(2.0, function() { SendToConsole("gameinstructor_reload_lessons; gameinstructor_enable 0; ent_fire env_instructor_hint EndHint; ent_fire env_instructor_hint Kill") })
SigfAfter(1.5, function() {
	local h = SigfHost()
	h.SetOrigin(Vector(-192.0, -1380.0, ::SK_FZ + 2.0))
	h.SetVelocity(Vector(0.0, 0.0, 0.0))
	SigfLook(4, 90)
	::SkStart()
})
SigfEvery(0.7, function() { ::SkDisco() })
SigfAfter(2.0, function() { ::SkIntro() })
SigfEvery(3.0, function() { ::SkMusic() })
