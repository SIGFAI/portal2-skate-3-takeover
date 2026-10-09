// Skate 3 Takeover: the player skates. Always rolling, ramps launch, tricks score, bad landings go to the Hall of Meat.
// The park floor and ramps have no collision: the skater is held on them by hand (see SkSurface in sigf_park.nut).
::Sk <- {
	on = false, auto = false, speed = 270.0, hdg = 90.0, dirx = 0.0, diry = 1.0, lastx = 0.0, lasty = 0.0, lastT = 0.0, rideVz = 0.0,
	ridden = false, air = false, airT0 = 0.0, airT = 0.5, trickKind = 0, trickPts = 50, trickName = "OLLIE", spinTurns = 1, portal = false,
	grind = null, grindT = 0.0, grindDir = 1.0, grindNext = 0.0, grindPts = 0,
	score = 0, combo = 1, bailUntil = 0.0, bailStart = 0.0, bails = 0, bones = 0, lastLand = 0.0, tricks = 0,
	route = null, wi = 0, lastYaw = 90.0, mouse = 0.0, stuck = 0.0, autoSince = 0.0, forceBail = false, gapTicks = 0, arm = false, lastPos = null, yawOff = 0.0, roll = 0.0, pitch = 6.0, noPortalUntil = 0.0, lean = 0.0, portalT = -10.0
}

::SkTricks <- [
	["OLLIE", 50, 0], ["POP SHOVE-IT", 120, 1], ["KICKFLIP", 150, 3], ["HEELFLIP", 150, 3], ["VARIAL FLIP", 220, 3],
	["360 FLIP", 400, 1], ["TRE FLIP", 450, 3], ["HARDFLIP", 500, 2], ["BACKSIDE 360", 600, 1], ["720 SHOVE-IT", 900, 1]
]
::SkGravity <- 450.0
::SkLightFx <- null

::SkNum <- function(n) {
	local s = n.tostring()
	local out = ""
	local len = s.len()
	for (local i = 0; i < len; i++) {
		out += s.slice(i, i + 1)
		local rest = len - 1 - i
		if (rest > 0 && rest % 3 == 0) out += ","
	}
	return out
}

// Full-screen art for a moment (additive: black is see-through). A newer overlay replaces an older one.
::SkOvId <- 0
::SkOv <- function(name, sec) {
	::SkOvId++
	local id = ::SkOvId
	SigfCmd("r_drawscreenoverlay 1; r_screenoverlay sigf/" + name)
	SigfIn(sec, function():(id) { if (id == ::SkOvId) SigfCmd("r_screenoverlay off") })
}

::SkHud <- function() {
	local c = ::Sk.combo > 1 ? "   COMBO x" + ::Sk.combo : ""
	SigfText("SCORE " + ::SkNum(::Sk.score) + c, 0.03, 0.88, 3.0, "255 90 210", 3)
}

::SkPop <- function(text, color, y = 0.3, sec = 1.4) {
	SigfText(text, -1.0, y, sec, color, 4)
}

::SkMiles <- [[10000, "10K!  WARMED UP"], [30000, "30K!  SICK SESSION"], [60000, "60K!  PRO SKATER"], [100000, "100K!  PORTAL LEGEND"], [200000, "200K!  HALL OF FAME"]]
::SkMile <- 0
::SkCheckMiles <- function() {
	if (::SkMile >= ::SkMiles.len()) return
	if (::Sk.score >= ::SkMiles[::SkMile][0]) {
		SigfText("SPOT CHALLENGE COMPLETE: " + ::SkMiles[::SkMile][1], -1.0, 0.18, 3.5, "255 235 70", 4)
		SigfSound("sigf/cheer.wav")
		::SkOv("ov_star", 1.2)
		::SkMile++
	}
}

::SkAddScore <- function(pts) {
	::Sk.score += pts * ::Sk.combo
	::SkHud()
	::SkCheckMiles()
}

// View angles. In auto mode the bot owns them (heading + trick spin); a human keeps the mouse (we only add the roll).
::SkSetView <- function() {
	local p = ::Sk.pitch
	local y = ::Sk.hdg + ::Sk.yawOff
	if (!::Sk.auto) {
		local f = SigfHost().GetForwardVector()
		y = atan2(f.y, f.x) * 57.29578
		p = -asin(f.z) * 57.29578
	}
	::Sk.lastYaw = y
	SendToConsole(format("setang %.2f %.2f %.2f", p, y, ::Sk.roll))
}

::SkBail <- function() {
	local now = Time()
	::Sk.bailUntil = now + 1.7
	::Sk.bailStart = now
	::Sk.bails++
	local bones = RandomInt(4, 17)
	::Sk.bones += bones
	::Sk.combo = 1
	::Sk.air = false
	::Sk.grind = null
	::Sk.forceBail = false
	SigfSound("sigf/bail.wav")
	SigfSound("sigf/crowd_ooh.wav")
	SigfCmd("host_timescale 0.5")
	::SkXray()
	::SkFlash("255 25 25 9000", 1.3)
	::SkOv("ov_red", 0.3)
	SigfIn(0.3, function() { ::SkOv("ov_xray", 1.1) })
	SigfIn(1.5, function() { ::SkOv("ov_skull", 0.9) })
	SigfCaption("HALL OF MEAT!", 2.4)
	SigfText("BONES BROKEN: " + bones + "    +$" + (bones * 100), -1.0, 0.4, 2.2, "255 70 70", 4)
	::Sk.score += bones * 100
	::SkHud()
	::SkCheckMiles()
	SigfIn(0.9, function() { SigfCmd("host_timescale 1") })
	SigfIn(0.5, function() { SigfSound("sigf/v_ouch.wav") })
}

// X-ray: the whole chamber turns into neon wireframe for a moment (the Hall of Meat look).
::SkXray <- function() {
	SigfCmd("mat_wireframe 1")
	SigfIn(0.35, function() { SigfCmd("mat_wireframe 0") })
	SigfIn(0.5, function() { SigfCmd("mat_wireframe 1") })
	SigfIn(1.0, function() { SigfCmd("mat_wireframe 0") })
	SigfIn(1.1, function() { SigfCmd("mat_wireframe 1") })
	SigfIn(1.5, function() { SigfCmd("mat_wireframe 0") })
}

// A colored flash of light over the player.
::SkFlash <- function(rgb, sec) {
	local l = Entities.CreateByClassname("env_projectedtexture")
	local o = SigfHost().GetOrigin()
	l.SetOrigin(Vector(o.x, o.y, o.z + 80.0))
	l.SetAngles(90.0, 0.0, 0.0)
	EntFireByHandle(l, "LightColor", rgb, 0.0, null, null)
	EntFireByHandle(l, "FOV", "160", 0.0, null, null)
	EntFireByHandle(l, "TurnOn", "", 0.05, null, null)
	EntFireByHandle(l, "Kill", "", sec, null, null)
}

::SkStartGrind <- function(rail, x, y) {
	local dx = rail.x1 - rail.x0
	local dy = rail.y1 - rail.y0
	local len = sqrt(dx * dx + dy * dy)
	local ux = dx / len
	local uy = dy / len
	::Sk.grind = rail
	::Sk.grindDir = (::Sk.dirx * ux + ::Sk.diry * uy) >= 0 ? 1.0 : -1.0
	::Sk.grindT = (x - rail.x0) * ux + (y - rail.y0) * uy
	::Sk.grindNext = Time()
	::Sk.grindPts = 0
	::Sk.air = false
	::Sk.yawOff = 0.0
	::Sk.roll = 0.0
	SigfCmd("-left; -right; cl_yawspeed 210")
	SigfSound("sigf/grind.wav")
	::SkPop("50-50 GRIND", "60 255 130", 0.3, 1.4)
}

::SkGrindTick <- function(h, now, dt) {
	local g = ::Sk.grind
	local dx = g.x1 - g.x0
	local dy = g.y1 - g.y0
	local len = sqrt(dx * dx + dy * dy)
	local ux = dx / len
	local uy = dy / len
	::Sk.grindT += ::Sk.grindDir * ::Sk.speed * dt
	::Sk.hdg = atan2(uy * ::Sk.grindDir, ux * ::Sk.grindDir) * 57.29578
	if (::Sk.grindT < 0 || ::Sk.grindT > len) {
		local ex = g.x0 + ux * (::Sk.grindT < 0 ? 0 : len)
		local ey = g.y0 + uy * (::Sk.grindT < 0 ? 0 : len)
		::Sk.grind = null
		::Sk.dirx = ux * ::Sk.grindDir
		::Sk.diry = uy * ::Sk.grindDir
		h.SetOrigin(Vector(ex, ey, ::SK_FZ + g.h + 6.0))
		h.SetVelocity(Vector(::Sk.dirx * ::Sk.speed, ::Sk.diry * ::Sk.speed, 190.0))
		::SkAddScore(::Sk.grindPts)
		::SkPop("GRIND +" + ::SkNum(::Sk.grindPts * ::Sk.combo), "60 255 130", 0.3, 1.2)
		::Sk.combo = ::Sk.combo < 8 ? ::Sk.combo + 1 : 8
		SigfSound("sigf/trick_win.wav")
		::SkOv("ov_sk8", 0.8)
		::SkSparkLight(false)
		return
	}
	local px = g.x0 + ux * ::Sk.grindT
	local py = g.y0 + uy * ::Sk.grindT
	h.SetOrigin(Vector(px, py, ::SK_FZ + g.h + 2.0))
	h.SetVelocity(Vector(ux * ::Sk.grindDir * ::Sk.speed, uy * ::Sk.grindDir * ::Sk.speed, 0.0))
	::Sk.grindPts += 5
	::SkSparkLight(true, Vector(px, py, ::SK_FZ + g.h + 8.0))
	if (now > ::Sk.grindNext) { ::Sk.grindNext = now + 1.3; SigfSound("sigf/grind.wav") }
	::Sk.roll = 7.0 * sin(now * 9.0) + 5.0
	::SkSetView()
}

// Orange sparks while grinding: a flickering light that rides with the board.
::SkSparkLight <- function(on, pos = null) {
	if (!on) {
		if (::SkLightFx != null && ::SkLightFx.IsValid()) EntFireByHandle(::SkLightFx, "TurnOff", "", 0.0, null, null)
		return
	}
	if (::SkLightFx == null || !::SkLightFx.IsValid()) {
		local l = Entities.CreateByClassname("env_projectedtexture")
		l.SetAngles(90.0, 0.0, 0.0)
		EntFireByHandle(l, "FOV", "120", 0.0, null, null)
		::SkLightFx = l
	}
	::SkLightFx.SetOrigin(pos + Vector(0.0, 0.0, 70.0))
	local b = RandomInt(0, 2) == 0 ? "255 200 80 2600" : "255 120 30 1600"
	EntFireByHandle(::SkLightFx, "LightColor", b, 0.0, null, null)
	EntFireByHandle(::SkLightFx, "TurnOn", "", 0.0, null, null)
}

::SkPickTrick <- function(air) {
	local pool = []
	foreach (t in ::SkTricks) {
		local need = t[1] < 100 ? 0.0 : (t[1] < 300 ? 0.25 : (t[1] < 550 ? 0.55 : 0.85))
		if (air >= need) pool.append(t)
	}
	if (pool.len() == 0) return ::SkTricks[0]
	local idx = pool.len() - 1 - RandomInt(0, pool.len() > 2 ? 2 : pool.len() - 1)
	return pool[idx < 0 ? 0 : idx]
}

::SkTakeoff <- function(h, now, vz) {
	::Sk.air = true
	::Sk.rideVz = 0.0
	::Sk.airT0 = now
	::Sk.roll = 0.0
	::Sk.yawOff = 0.0
	::Sk.trickKind = 0
	::Sk.trickPts = 50
	::Sk.trickName = "OLLIE"
	local T = vz > 100 ? (vz / ::SkGravity) * 2.0 : 0.35
	::Sk.airT = T
	if (T > 0.45) {
		local t = ::SkPickTrick(T)
		::Sk.trickName = t[0]
		::Sk.trickKind = t[2]
		::Sk.trickPts = t[1]
		::Sk.spinTurns = T > 0.95 ? 2 : 1
		if (!::Sk.auto && ::Sk.trickKind == 1) SigfCmd("cl_yawspeed " + (360.0 * ::Sk.spinTurns / T).tointeger() + "; +left")
		SigfSound("sigf/ollie_pop.wav")
		::SkPop(t[0], "255 235 70", 0.26, T + 0.5)
	} else {
		SigfSound("sigf/ollie_pop.wav")
	}
}

::SkLand <- function(h, now, vz) {
	::Sk.air = false
	::Sk.rideVz = 0.0
	SigfCmd("-left; -right; cl_yawspeed 210")
	::Sk.yawOff = 0.0
	::Sk.roll = 0.0
	local t = now - ::Sk.airT0
	if (t < 0.12) return
	::Sk.lastLand = now
	if (::Sk.forceBail || (vz < -620.0 && t > 1.0)) {
		::SkBail()
		return
	}
	SigfSound("sigf/land.wav")
	local pts = ::Sk.trickPts
	local name = ::Sk.trickName
	if (now - ::Sk.portalT < 5.0) { pts += 1500; name = "PORTAL AIR " + name; ::Sk.portalT = -10.0 }
	pts = (pts * (0.6 + t)).tointeger()
	::Sk.tricks++
	::Sk.combo = ::Sk.combo < 8 ? ::Sk.combo + 1 : 8
	::SkAddScore(pts)
	::SkPop(name + "  +" + ::SkNum(pts * ::Sk.combo), "90 255 255", 0.36, 1.5)
	if (name.find("PORTAL") != null) { ::SkOv("ov_burst", 0.9); SigfSound("sigf/cheer.wav") }
	else if (t > 0.8) ::SkOv(RandomInt(0, 1) == 0 ? "ov_sick" : "ov_bolt", 0.8)
	if (t > 0.6) SigfSound(RandomInt(0, 1) == 0 ? "sigf/v_sick.wav" : "sigf/v_gnarly.wav")
	else SigfSound("sigf/trick_win.wav")
}

// Space bar (bound to exec sk_jump): an ollie off the virtual floor.
::SkOllie <- function() {
	if (!::Sk.on) return
	if (::Sk.auto && !::SigfDemoMode && Time() - ::Sk.autoSince > 1.5) {
		::Sk.auto = false
		::Sk.yawOff = 0.0
		::Sk.roll = 0.0
		SigfText("YOUR BOARD NOW:  mouse steers,  W / S speed,  SPACE ollies", -1.0, 0.8, 4.0, "255 255 255", 3)
	}
	if (!::Sk.ridden) return
	local h = SigfHost()
	local v = h.GetVelocity()
	h.SetVelocity(Vector(v.x, v.y, 260.0))
	::Sk.ridden = false
}

// The bot: steer to the waypoints of ::Sk.route = [[x, y, speed, flag], ...]; flag "jump" ollies at that point.
::SkAutoSteer <- function(x, y, dt) {
	local r = ::Sk.route
	if (r == null || r.len() == 0) return
	local wp = r[::Sk.wi]
	local dx = wp[0] - x
	local dy = wp[1] - y
	if (dx * dx + dy * dy < 70.0 * 70.0) {
		if (wp.len() > 3 && wp[3] == "jump" && ::Sk.ridden) {
			local v = SigfHost().GetVelocity()
			SigfHost().SetVelocity(Vector(v.x, v.y, 260.0))
			::Sk.ridden = false
		}
		if (wp.len() > 3 && wp[3] == "bail") ::Sk.forceBail = true
		::Sk.wi = (::Sk.wi + 1) % r.len()
		wp = r[::Sk.wi]
		dx = wp[0] - x
		dy = wp[1] - y
	}
	local want = atan2(dy, dx) * 57.29578
	local d = want - ::Sk.hdg
	while (d > 180.0) d -= 360.0
	while (d < -180.0) d += 360.0
	local mx = 130.0 * dt
	if (d > mx) d = mx
	if (d < -mx) d = -mx
	::Sk.hdg += d
	if (!::Sk.air && ::Sk.grind == null) ::Sk.lean = d / dt * -0.045
	::Sk.speed += (wp[2] - ::Sk.speed) * 0.04
}

// Called about 30 times a second.
::SkPlayerTick <- function() {
	if (!::Sk.on) return
	local h = SigfHost()
	if (h == null) return
	local now = Time()
	local dt = now - ::Sk.lastT
	if (::Sk.lastT == 0.0 || dt > 0.1 || dt < 0.005) dt = 0.03
	::Sk.lastT = now
	local o = h.GetOrigin()
	local x = o.x
	local y = o.y
	local z = o.z
	local v = h.GetVelocity()
	local vx = v.x
	local vy = v.y
	local vz = v.z
	// a real portal moved the player across the park: turn the heading with the exit and mark the air
	if (::Sk.lastPos != null) {
		local jx = x - ::Sk.lastPos[0]
		local jy = y - ::Sk.lastPos[1]
		if (jx * jx + jy * jy > 250.0 * 250.0 && now > ::Sk.noPortalUntil) {
			::Sk.noPortalUntil = now + 1.0
			local hs = sqrt(vx * vx + vy * vy)
			if (hs > 20.0) {
				::Sk.hdg = atan2(vy, vx) * 57.29578
				::Sk.dirx = vx / hs
				::Sk.diry = vy / hs
			}
			::Sk.portalT = now
			::Sk.lastYaw = ::Sk.hdg
			::Sk.mouse = 0.0
			if (::Sk.route != null && ::Sk.route[::Sk.wi].len() > 3 && ::Sk.route[::Sk.wi][3] == "portal") ::Sk.wi = (::Sk.wi + 1) % ::Sk.route.len()
			::SkPop("PORTAL!", "60 170 255", 0.2, 1.3)
			SigfSound("sigf/trick_win.wav")
		}
	}
	::Sk.lastPos = [x, y]
	if (now < ::Sk.bailUntil) {
		// tumbling: rolling over and over, sliding to a stop
		local k = (now - ::Sk.bailStart) / 1.7
		if (k > 1.0) k = 1.0
		::Sk.roll = 1080.0 * k * (1.0 - k) * 1.9
		::Sk.pitch = 6.0 + 40.0 * sin(k * 3.14159)
		::SkSetView()
		local s = 1.0 - k
		h.SetVelocity(Vector(::Sk.dirx * 180.0 * s, ::Sk.diry * 180.0 * s, vz < 5.0 ? 0.0 : vz))
		return
	}
	::Sk.pitch = 6.0
	if (::Sk.grind != null) { ::SkGrindTick(h, now, dt); return }
	if (::Sk.auto) {
			::SkAutoSteer(x, y, dt)
			// stuck against a wall: skip to the next waypoint
			local hs = sqrt(vx * vx + vy * vy)
			if (hs < 70.0 && ::Sk.ridden) ::Sk.stuck += dt else ::Sk.stuck = 0.0
			if (::Sk.stuck > 0.8) {
				::Sk.stuck = 0.0
				::Sk.wi = (::Sk.wi + 1) % ::Sk.route.len()
				h.SetOrigin(Vector(-192.0, -1380.0, ::SK_FZ + 2.0))
				h.SetVelocity(Vector(0.0, 0.0, 0.0))
				::Sk.hdg = 90.0
				::Sk.wi = 1
			}
	}
	// the north side of the park is open to the room: stop there (the bot turns away on its own)
	if (y > ::SK_Y1 - 14.0) {
		y = ::SK_Y1 - 14.0
		if (vy > 0.0) vy = 0.0
		h.SetOrigin(Vector(x, y, z))
		h.SetVelocity(Vector(vx, vy, vz))
	}
	local surf = ::SkSurface(x, y)
	if (z <= surf + 2.0 && vz < 230.0) {
		local nxt = ::SkSurface(x + vx * dt, y + vy * dt)
		local slope = (nxt - surf) / dt
		local want = 0.0
		if (nxt - surf < -14.0 || (::Sk.rideVz > 60.0 && slope < ::Sk.rideVz - 90.0)) want = ::Sk.rideVz   // lip or crest of a ramp
		else {
			want = (nxt - z) / dt
			if (want > 380.0) want = 380.0
			if (want < -380.0) want = -380.0
			::Sk.rideVz = want > 0.0 ? want : 0.0
		}
		vz = want
		h.SetVelocity(Vector(vx, vy, vz))
		::Sk.ridden = true
	} else {
		::Sk.ridden = false
	}
	local grounded = ::Sk.ridden
	if (!grounded && !::Sk.air && ::Sk.gapTicks >= 1 && ::Sk.arm) ::SkTakeoff(h, now, vz)
	if (!grounded) ::Sk.gapTicks++
	else { ::Sk.gapTicks = 0; ::Sk.arm = true }
	if (grounded && ::Sk.air) ::SkLand(h, now, vz)
	// rails catch a skater who is above them
	if (::Sk.air || !grounded) {
		foreach (r in ::SkRails) {
			local dx = r.x1 - r.x0
			local dy = r.y1 - r.y0
			local len = sqrt(dx * dx + dy * dy)
			local ux = dx / len
			local uy = dy / len
			local t = (x - r.x0) * ux + (y - r.y0) * uy
			local d = fabs(-(x - r.x0) * uy + (y - r.y0) * ux)
			local rz = ::SK_FZ + r.h
			if (t > 0 && t < len && d < 38.0 && z > rz - 12.0 && z < rz + 36.0 && vz < 100.0) {
				::SkStartGrind(r, x, y)
				return
			}
		}
	}
	if (grounded) {
		local tx = ::Sk.dirx
		local ty = ::Sk.diry
		if (::Sk.auto) {
			tx = cos(::Sk.hdg / 57.29578)
			ty = sin(::Sk.hdg / 57.29578)
			local ln = ::Sk.lean
			if (ln > 9.0) ln = 9.0
			if (ln < -9.0) ln = -9.0
			::Sk.roll += (ln - ::Sk.roll) * 0.25
		} else {
			local f = h.GetForwardVector()
			local fl = sqrt(f.x * f.x + f.y * f.y)
			if (fl > 0.2) { tx = f.x / fl; ty = f.y / fl }
			// W / S show up as speed the engine added by itself since our last push
			local dv = (vx - ::Sk.lastx) * ::Sk.dirx + (vy - ::Sk.lasty) * ::Sk.diry
			if (dv > 10.0) ::Sk.speed = ::Sk.speed + 6.0
			else if (dv < -10.0) ::Sk.speed = ::Sk.speed - 9.0
			else ::Sk.speed = ::Sk.speed + (270.0 - ::Sk.speed) * 0.01
			if (::Sk.speed > 470.0) ::Sk.speed = 470.0
			if (::Sk.speed < 140.0) ::Sk.speed = 140.0
		}
		::Sk.dirx = ::Sk.dirx + (tx - ::Sk.dirx) * 0.35
		::Sk.diry = ::Sk.diry + (ty - ::Sk.diry) * 0.35
		local dl = sqrt(::Sk.dirx * ::Sk.dirx + ::Sk.diry * ::Sk.diry)
		::Sk.dirx /= dl
		::Sk.diry /= dl
		::Sk.lastx = ::Sk.dirx * ::Sk.speed
		::Sk.lasty = ::Sk.diry * ::Sk.speed
		h.SetVelocity(Vector(::Sk.lastx, ::Sk.lasty, vz))
		if (now - ::Sk.lastLand > 7.0 && ::Sk.combo > 1) ::Sk.combo = 1
	} else {
		// airborne: flips roll the view, shove-its spin it
		local t = (now - ::Sk.airT0) / ::Sk.airT
		if (t > 1.0) t = 1.0
		if (::Sk.trickKind == 3) ::Sk.roll = 360.0 * t
		else if (::Sk.trickKind == 2) { ::Sk.roll = 360.0 * t; ::Sk.pitch = 6.0 - 40.0 * sin(t * 3.14159) }
		else if (::Sk.trickKind == 1 && ::Sk.auto) ::Sk.yawOff = 360.0 * ::Sk.spinTurns * t
		// lighter gravity than the game's 600, without touching the server cvar (it prints a notice on screen)
		vz += (600.0 - ::SkGravity) * dt
		if (::Sk.auto) h.SetVelocity(Vector(::Sk.dirx * ::Sk.speed, ::Sk.diry * ::Sk.speed, vz))
		else h.SetVelocity(Vector(vx, vy, vz))
		::Sk.lastx = vx
		::Sk.lasty = vy
	}
	if (::Sk.auto || ::Sk.air || ::Sk.roll != 0.0) ::SkSetView()
	if (z < ::SK_FZ - 350.0) {
		h.SetOrigin(Vector(-192.0, -1380.0, ::SK_FZ + 4.0))
		h.SetVelocity(Vector(0.0, 0.0, 0.0))
		::Sk.air = false
	}
}

::SkStart <- function() {
	::Sk.on = true
	::Sk.auto = !::SigfDemoMode
	::Sk.route = ::SkRoute
	::Sk.wi = 1
	::Sk.hdg = 90.0
	::Sk.autoSince = Time()
	SigfAutoCam(false)
	SigfCmd("bind space \"+jump; exec sk_jump\"; sv_friction 0; sv_stopspeed 0; fov_desired 105; sv_airaccelerate 4")
	::SkFastFns.append(::SkPlayerTick)
}
