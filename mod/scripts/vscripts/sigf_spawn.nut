// Skate 3 Takeover: serial prop spawner (console spawn, adopt on a later frame).
::SkQ <- []
::SkWait <- null
::SkFind <- function(model, before) {
	local e = null
	local found = null
	while ((e = Entities.FindByModel(e, model)) != null) {
		local old = false
		foreach (b in before) { if (b == e) { old = true; break } }
		if (!old) found = e
	}
	return found
}
// One step per call: either adopt the entity the last command created, or send the next command.
::SkQTick <- function() {
	if (::SkWait != null) {
		local w = ::SkWait
		local e = ::SkFind(w.model, w.before)
		if (e == null) {
			w.tries++
			if (w.tries < 20) return    // the model may still be loading
			::SkWait = null
			printl("SIGF_WARN spawn failed " + w.model)
		} else {
			::SkWait = null
			e.SetOrigin(w.pos)
			if (w.ang != null) e.SetAngles(w.ang.x, w.ang.y, w.ang.z)
			else e.SetAngles(0.0, 0.0, 0.0)
			if (w.solid == false) EntFireByHandle(e, "DisableCollision", "", 0.0, null, null)
			if (w.phys) EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
			if (w.scale != 1.0) EntFireByHandle(e, "AddOutput", "modelscale " + w.scale, 0.0, null, null)
			if (w.color != null) EntFireByHandle(e, "Color", w.color, 0.0, null, null)
			if (w.life > 0) EntFireByHandle(e, "Kill", "", w.life, null, null)
			if (w.fn != null) ::SigfCall("spawn callback", w.fn, e)
		}
	}
	if (::SkQ.len() == 0 || ::SkWait != null) return
	local it = ::SkQ[0]
	::SkQ.remove(0)
	local before = []
	local e = null
	while ((e = Entities.FindByModel(e, it.model)) != null) before.append(e)
	it.before <- before
	it.tries <- 0
	::SkWait = it
	local arg = it.model.slice(7, it.model.len() - 4)
	SendToConsole((it.phys ? "prop_physics_create " : "prop_dynamic_create ") + arg)
}
// SkMake("models/x.mdl", pos, { ang = Vector(p,y,r), scale = 1.0, color = "r g b", phys = false, life = 0, fn = function(e) {} })
::SkMake <- function(model, pos, o = {}) {
	::SkQ.append({ model = model, pos = pos, phys = "phys" in o ? o.phys : false, ang = "ang" in o ? o.ang : null,
		scale = "scale" in o ? o.scale : 1.0, color = "color" in o ? o.color : null,
		life = "life" in o ? o.life : 0.0, solid = "solid" in o ? o.solid : true, fn = "fn" in o ? o.fn : null })
}
// Fast clock (30 Hz) for the spawner and the skaters.
::SkFastFns <- []
::SkFast <- function() {
	::SkQTick()
	foreach (f in ::SkFastFns) ::SigfCall("fast", f)
}
::SkStartFast <- function() {
	local old = Entities.FindByName(null, "sk_fast")
	if (old != null) old.Destroy()
	local t = Entities.CreateByClassname("logic_timer")
	t.__KeyValueFromString("targetname", "sk_fast")
	t.ValidateScriptScope()
	t.GetScriptScope().SkOnTimer <- function() { ::SkFast() }
	t.ConnectOutput("OnTimer", "SkOnTimer")
	EntFireByHandle(t, "RefireTime", "0.03", 0.0, null, null)
	EntFireByHandle(t, "Enable", "", 0.0, null, null)
}
