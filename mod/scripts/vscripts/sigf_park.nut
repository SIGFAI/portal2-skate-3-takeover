// Skate 3 Takeover: the skate park (built from Portal 2's own floor plates and rails).
// World layout: floor top FZ, park corner (X0, Y0), 6 x 6 plates of 192 units.
::SK_FZ <- -370.0
::SK_OFF <- 5.4      // the plates walk 5.4 units above their origin
::SK_X0 <- -832.0
::SK_X1 <- 576.0
::SK_Y1 <- -288.0
::SK_Y0 <- -1440.0
::SK_PL <- "models/props_gameplay/crusher_floorplate.mdl"
::SK_RAIL <- "models/props_bts/rail_horizontal_straight_128.mdl"
::SkRamps <- []     // { x, y, dir (deg, direction of the rise), hw, half, h0, h1 }
::SkRails <- []     // { x0, y0, x1, y1, h }  rail top above the floor
::SkBuilt <- false

::SkRad <- function(d) { return d / 57.29578 }

// Height of the walkable surface at (x, y), floor included. Boxes of the park are only ramps.
::SkSurface <- function(x, y) {
	local top = ::SK_FZ
	foreach (r in ::SkRamps) {
		local a = ::SkRad(r.dir)
		local c = cos(a)
		local s = sin(a)
		local dx = x - r.x
		local dy = y - r.y
		local u = dx * c + dy * s
		local v = -dx * s + dy * c
		if (fabs(v) < r.hw && u > -r.half && u < r.half) {
			local t = (u + r.half) / (2.0 * r.half)
			local h = ::SK_FZ + r.h0 + (r.h1 - r.h0) * t
			if (h > top) top = h
		}
	}
	return top
}

// One ramp made of floor plates pitched along dir. rise is the height gained over one plate.
::SkRamp <- function(x, y, dir, lanes, h0, h1, color) {
	local run = 180.0
	local ang = atan2(h1 - h0, run) * 57.29578
	local half = 96.0 * cos(::SkRad(fabs(ang)))
	::SkRamps.append({ x = x, y = y, dir = dir, hw = 96.0 * lanes, half = half, h0 = h0, h1 = h1 })
	local a = ::SkRad(dir)
	for (local k = 0; k < lanes; k++) {
		local off = (k - (lanes - 1) * 0.5) * 192.0
		local px = x + (-sin(a)) * off
		local py = y + cos(a) * off
		::SkMake(::SK_PL, Vector(px, py, ::SK_FZ + (h0 + h1) * 0.5 - ::SK_OFF), { ang = Vector(-ang, dir, 0.0), color = color, solid = false })
	}
}

::SkRail <- function(x0, y0, x1, y1, h, color) {
	::SkRails.append({ x0 = x0, y0 = y0, x1 = x1, y1 = y1, h = h })
	local dx = x1 - x0
	local dy = y1 - y0
	local len = sqrt(dx * dx + dy * dy)
	local yaw = atan2(dy, dx) * 57.29578
	local n = (len / 128.0 + 0.5).tointeger()
	if (n < 1) n = 1
	for (local i = 0; i < n; i++) {
		local t = (i + 0.5) / n
		::SkMake(::SK_RAIL, Vector(x0 + dx * t, y0 + dy * t, ::SK_FZ + h), { ang = Vector(0.0, yaw, 0.0), color = color, solid = false })
	}
}

// Colored light (a projected texture shining down) so the neon pops in the dark chamber.
::SkLights <- []
::SkLight <- function(pos, rgb, fov, far) {
	local l = Entities.CreateByClassname("env_projectedtexture")
	l.SetOrigin(pos)
	l.SetAngles(90.0, 0.0, 0.0)
	EntFireByHandle(l, "LightColor", rgb, 0.0, null, null)
	EntFireByHandle(l, "FOV", fov.tostring(), 0.0, null, null)
	EntFireByHandle(l, "TurnOn", "", 0.1, null, null)
	::SkLights.append(l)
	return l
}

::SkNeon <- [["255 40 200 4500", "40 220 255 4500", "255 230 60 4500", "160 70 255 4500"],
	["40 220 255 4500", "255 230 60 4500", "160 70 255 4500", "255 40 200 4500"],
	["255 230 60 4500", "160 70 255 4500", "255 40 200 4500", "40 220 255 4500"],
	["160 70 255 4500", "255 40 200 4500", "40 220 255 4500", "255 230 60 4500"]]
::SkNeonI <- 0
::SkDisco <- function() {
	::SkNeonI = (::SkNeonI + 1) % 4
	local pal = ::SkNeon[::SkNeonI]
	foreach (i, l in ::SkLights) {
		if (l.IsValid()) EntFireByHandle(l, "LightColor", pal[i % 4], 0.0, null, null)
	}
}

// Real, linked Portal 2 portals on two walls: blue on the west wall, orange on the south wall (it shoots straight into the kicker).
::SkPlacePortals <- function() {
	local z = ::SK_FZ + 50.0
	SigfCmd("portal_place 0 0 -830.5 -700 " + z + " 0 0 0")
	SigfCmd("portal_place 0 1 192 -1438.5 " + z + " 0 90 0")
}

::SkBuildPark <- function() {
	if (::SkBuilt) return
	::SkBuilt = true
	::SkPlacePortals()
	// two kickers, a pyramid in the middle, rails after each kicker
	::SkRamp(-576.0, -1090.0, 90.0, 1, 0.0, 72.0, "255 60 200")
	::SkRamp(192.0, -1090.0, 90.0, 1, 0.0, 72.0, "40 230 255")
	::SkRamp(-192.0, -870.0, 90.0, 2, 0.0, 72.0, "255 200 40")
	::SkRamp(-192.0, -690.0, 90.0, 2, 72.0, 0.0, "255 200 40")
	::SkRail(-576.0, -880.0, -576.0, -520.0, 38.0, "40 255 120")
	::SkRail(192.0, -880.0, 192.0, -520.0, 38.0, "255 120 40")
	::SkLight(Vector(-576.0, -900.0, ::SK_FZ + 300.0), "255 40 200 4500", 130, 800)
	::SkLight(Vector(192.0, -900.0, ::SK_FZ + 300.0), "40 220 255 4500", 130, 800)
	::SkLight(Vector(-192.0, -700.0, ::SK_FZ + 300.0), "255 230 60 4500", 130, 800)
	::SkLight(Vector(-192.0, -1300.0, ::SK_FZ + 300.0), "160 70 255 4500", 130, 800)
	::SkSpawnSkaters()
	// floor last: it builds as a wave around everything else
	for (local j = 0; j < 7; j++) {
		for (local i = 0; i < 9; i++) {
			local c = ((i + j) % 2 == 0) ? "110 150 255" : "255 120 220"
			::SkMake(::SK_PL, Vector(::SK_X0 + 96.0 + i * 168.0, ::SK_Y0 + 96.0 + j * 168.0, ::SK_FZ - ::SK_OFF), { color = c, solid = false })
		}
	}
}

// The bot's lap around the park (used in the demo): pyramid, west portal, kicker + rail, loop, bail.
::SkRoute <- [
	[-192.0, -1380.0, 300.0], [-192.0, -540.0, 345.0], [-450.0, -620.0, 340.0], [-620.0, -700.0, 345.0], [-900.0, -700.0, 350.0, "portal"],
	[192.0, -1200.0, 345.0], [192.0, -420.0, 330.0], [330.0, -330.0, 320.0], [440.0, -700.0, 330.0], [440.0, -1300.0, 330.0],
	[100.0, -1360.0, 320.0], [-576.0, -1350.0, 330.0], [-576.0, -1100.0, 340.0, "bail"], [-576.0, -420.0, 340.0], [-500.0, -330.0, 320.0],
	[-192.0, -400.0, 330.0], [-192.0, -1300.0, 330.0]
]
