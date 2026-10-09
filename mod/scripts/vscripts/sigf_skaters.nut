// Skate 3 Takeover: the other skaters (Aperture characters on wheeled hover-tiles).
// They are driven by hand (no collision): follow waypoints, ride the ramps, grind rails, throw tricks, sometimes bail.
::SkSkaters <- []
::SkG <- 450.0

// Builds one skater at pos: a tinted tile board with four wheels and a rider on top.
::SkNewSkater <- function(name, riderModel, riderScale, riderLift, boardColor, path, speed) {
	local start = path[0]
	local s = { name = name, board = null, rider = null, ready = false,
		x = start[0], y = start[1], z = ::SK_FZ, hdg = 90.0, speed = speed, baseSpeed = speed, vx = 0.0, vy = 0.0, vz = 0.0, rideVz = 0.0,
		air = false, t0 = 0.0, T = 0.6, kind = 0, spins = 1.0, state = 0, until = 0.0, last = 0.0,
		path = path, wi = 1, grind = null, gT = 0.0, gDir = 1.0, bailChance = 0.14, tricks = 0, tumble = 0.0, hop = 0.0, label = "" }
	local p = Vector(start[0], start[1], ::SK_FZ + 11.0)
	::SkMake(::SK_PL, p, { scale = 0.34, color = boardColor, solid = false, fn = function(e):(s) { s.board = e } })
	local wheelOff = [[26.0, 18.0], [26.0, -18.0], [-26.0, 18.0], [-26.0, -18.0]]
	foreach (w in wheelOff) {
		::SkMake("models/br_debris/train_wheel.mdl", p + Vector(w[0], w[1], -4.0), { scale = 0.65, color = "30 30 30", solid = false, ang = Vector(90.0, 0.0, 0.0),
			fn = function(e):(s) { EntFireByHandle(e, "SetParent", "!activator", 0.0, s.board, null) } })
	}
	::SkMake(riderModel, p + Vector(0.0, 0.0, riderLift), { scale = riderScale, solid = false,
		fn = function(e):(s) { s.rider = e; EntFireByHandle(e, "SetParent", "!activator", 0.0, s.board, null); s.ready = true } })
	::SkSkaters.append(s)
	return s
}

::SkAngDiff <- function(a, b) {
	local d = a - b
	while (d > 180.0) d -= 360.0
	while (d < -180.0) d += 360.0
	return d
}

::SkSkaterPlace <- function(s, pitch, roll, yawOff) {
	if (s.board == null || !s.board.IsValid()) return
	s.board.SetOrigin(Vector(s.x, s.y, s.z + 11.0))
	s.board.SetAngles(pitch, s.hdg + yawOff, roll)
}

::SkSkaterLaunch <- function(s, now, vz) {
	s.air = true
	s.vz = vz
	s.t0 = now
	s.T = 2.0 * vz / ::SkG
	if (s.T < 0.3) s.T = 0.3
	s.kind = RandomInt(1, 3)
	s.spins = RandomInt(1, s.T > 0.9 ? 2 : 1)
	s.vx = cos(s.hdg / 57.29578) * s.speed
	s.vy = sin(s.hdg / 57.29578) * s.speed
}

::SkSkaterStart <- function(s, now, bail) {
}

::SkStepSkater <- function(s, now) {
	local dt = now - s.last
	if (s.last == 0.0 || dt > 0.1) dt = 0.03
	s.last = now
	if (s.board == null || !s.board.IsValid()) return
	// tumbling after a bail
	if (s.state == 2) {
		s.tumble += dt
		local k = s.tumble
		s.x += s.vx * dt * 0.5
		s.y += s.vy * dt * 0.5
		s.vx *= 0.96
		s.vy *= 0.96
		local sf = ::SkSurface(s.x, s.y)
		s.z = sf + 14.0 * fabs(sin(k * 6.0))
		::SkSkaterPlace(s, 360.0 * k * 1.3, 300.0 * k, 540.0 * k)
		if (now > s.until) { s.state = 0; s.tumble = 0.0; s.z = ::SkSurface(s.x, s.y) }
		return
	}
	// grinding a rail
	if (s.state == 1) {
		local g = s.grind
		local dx = g.x1 - g.x0
		local dy = g.y1 - g.y0
		local len = sqrt(dx * dx + dy * dy)
		s.gT += s.gDir * s.speed * dt
		if (s.gT < 0.0 || s.gT > len) {
			s.state = 0
			s.air = true
			s.vz = 160.0
			s.t0 = now
			s.T = 0.7
			s.kind = 0
			s.vx = dx / len * s.gDir * s.speed
			s.vy = dy / len * s.gDir * s.speed
			s.hdg = atan2(s.vy, s.vx) * 57.29578
			s.grind = null
			return
		}
		s.x = g.x0 + dx / len * s.gT
		s.y = g.y0 + dy / len * s.gT
		s.z = ::SK_FZ + g.h
		::SkSkaterPlace(s, 0.0, 0.0, 0.0)
		return
	}
	// steer toward the next waypoint
	local wp = s.path[s.wi]
	local dx = wp[0] - s.x
	local dy = wp[1] - s.y
	if (dx * dx + dy * dy < 80.0 * 80.0) {
		// a waypoint inside a portal: come out of the other one with the same speed
		if (wp.len() > 2 && !s.air) {
			if (wp[2] == "pA") { s.x = 192.0; s.y = -1405.0; s.hdg = 90.0 }
			else if (wp[2] == "pB") { s.x = -790.0; s.y = -700.0; s.hdg = 0.0 }
			::SkSkaterPlace(s, 0.0, 0.0, 0.0)
		}
		s.wi = (s.wi + 1) % s.path.len()
		wp = s.path[s.wi]
		dx = wp[0] - s.x
		dy = wp[1] - s.y
	}
	if (!s.air) {
		local want = atan2(dy, dx) * 57.29578
		local d = ::SkAngDiff(want, s.hdg)
		local mx = 150.0 * dt
		if (d > mx) d = mx
		if (d < -mx) d = -mx
		s.hdg += d
		local hr = s.hdg / 57.29578
		s.vx = cos(hr) * s.speed
		s.vy = sin(hr) * s.speed
		local nx = s.x + s.vx * dt
		local ny = s.y + s.vy * dt
		local surf = ::SkSurface(s.x, s.y)
		local nxt = ::SkSurface(nx, ny)
		local slope = (nxt - surf) / dt
		if (nxt - surf < -14.0 || (s.rideVz > 60.0 && slope < s.rideVz - 90.0)) {
			::SkSkaterLaunch(s, now, s.rideVz)
		} else {
			s.rideVz = slope > 0.0 ? slope : 0.0
			s.z = nxt
			s.x = nx
			s.y = ny
			// a random hop on flat ground
			if (nxt <= ::SK_FZ + 0.3 && RandomInt(0, 140) == 0) ::SkSkaterLaunch(s, now, 230.0)
		}
		::SkSkaterPlace(s, 0.0, 0.0, 0.0)
		return
	}
	// airborne
	s.vz -= ::SkG * dt
	s.x += s.vx * dt
	s.y += s.vy * dt
	s.z += s.vz * dt
	local t = (now - s.t0) / s.T
	if (t > 1.0) t = 1.0
	local pitch = 0.0
	local roll = 0.0
	local yaw = 0.0
	if (s.kind == 1) yaw = 360.0 * s.spins * t
	else if (s.kind == 2) pitch = 360.0 * t
	else if (s.kind == 3) roll = 360.0 * t
	// rails catch a skater coming down
	if (s.vz < 40.0) {
		foreach (r in ::SkRails) {
			local rx = r.x1 - r.x0
			local ry = r.y1 - r.y0
			local len = sqrt(rx * rx + ry * ry)
			local ux = rx / len
			local uy = ry / len
			local tt = (s.x - r.x0) * ux + (s.y - r.y0) * uy
			local d = fabs(-(s.x - r.x0) * uy + (s.y - r.y0) * ux)
			local rz = ::SK_FZ + r.h
			if (tt > 0 && tt < len && d < 40.0 && s.z > rz - 12.0 && s.z < rz + 36.0) {
				s.state = 1
				s.grind = r
				s.gT = tt
				s.gDir = (s.vx * ux + s.vy * uy) >= 0 ? 1.0 : -1.0
				s.air = false
				s.rideVz = 0.0
				::SkSkaterPlace(s, 0.0, 0.0, 0.0)
				return
			}
		}
	}
	local sf = ::SkSurface(s.x, s.y)
	if (s.z <= sf && s.vz <= 0.0) {
		s.z = sf
		s.air = false
		s.rideVz = 0.0
		s.tricks++
		if (s.T > 0.5 && RandomFloat(0.0, 1.0) < s.bailChance) {
			s.state = 2
			s.tumble = 0.0
			s.until = now + 1.5
			::SkSkaterPlace(s, 0.0, 0.0, 0.0)
			return
		}
		::SkSkaterPlace(s, 0.0, 0.0, 0.0)
		return
	}
	::SkSkaterPlace(s, pitch, roll, yaw)
}

::SkSkatersTick <- function() {
	local now = Time()
	foreach (s in ::SkSkaters) {
		if (s.ready) ::SkStepSkater(s, now)
	}
}

::SkSpawnSkaters <- function() {
	::SkNewSkater("Wheatley", "models/npcs/personality_sphere/personality_sphere.mdl", 1.4, 34.0, "255 60 200",
		[[-576.0, -1350.0], [-576.0, -520.0], [-560.0, -330.0], [-450.0, -560.0], [-620.0, -700.0], [-880.0, -700.0, "pA"], [192.0, -1250.0], [192.0, -520.0],
		[200.0, -330.0], [420.0, -420.0], [420.0, -1300.0], [100.0, -1350.0], [-300.0, -1350.0]], 330.0)
	::SkNewSkater("Angry Wheatley", "models/npcs/personality_sphere_angry.mdl", 1.4, 34.0, "40 230 255",
		[[192.0, -1350.0], [192.0, -1470.0, "pB"], [-500.0, -700.0], [-420.0, -400.0], [-420.0, -1300.0], [-192.0, -1350.0]], 310.0)
	::SkNewSkater("Turret", "models/props/turret_01.mdl", 1.1, 6.0, "255 230 40",
		[[-192.0, -1350.0], [-192.0, -330.0], [-100.0, -330.0], [300.0, -500.0], [300.0, -1300.0], [-100.0, -1350.0]], 290.0)
	::SkFastFns.append(::SkSkatersTick)
}
