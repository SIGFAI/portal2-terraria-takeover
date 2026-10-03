// Terraria Takeover: Terraria's slimes, demon eyes and bosses invade a Portal 2 test chamber.
// A block-throwing hero fights back with Portal 2's own cubes (dirt, stone, sand, dynamite). Blocks that
// fall into a real floor portal come out of the other one at full speed, right under the monster.
// Portal 2 scripts cannot spawn sprites or particle systems, so every effect (damage numbers, hearts,
// gel splashes, dust) is an env_smokestack that emits our own pixel-art materials.

::SigfStagePos = Vector(-62.0, -1400.0, -447.0)
::SigfStageAng = Vector(3.0, 90.0, 0.0)
SigfAutoCam(false)

::TT <- {
	FZ = -447.0          // floor height of the walkway
	X0 = -196.0          // lane edges (the torch pillars stand just outside)
	X1 = 72.0
	PX = -62.0           // where the hero stands
	PY = -1400.0
	FARY = -1040.0       // where enemies come in
	G = 700.0
	foes = []
	kills = 0
	blocks = []
	cubes = {}
	rps = []
	last = 0.0
	t0 = 0.0
	ready = false
	life = 400
	nextThrow = 0.0
	hudAt = 0.0
	look = 0.0
	portalUntil = 0.0
	item = "Dirt Block"
	itemAt = 0.0
	hurtAt = 0.0
	dressed = false
	music = 0.0
	director = true
	step = 0
	nextStep = 0.0
}

::TTKinds <- {
	slime = { model = "models/props/metal_box_fx_fizzler.mdl", scale = 1.0, r = 40.0, hp = 34, col = "70 140 255", name = "Blue Slime", flash = "255 90 90", fx = "sigf/pslime_blue.vmt" },
	greenslime = { model = "models/props/metal_box_fx_fizzler.mdl", scale = 0.8, r = 32.0, hp = 26, col = "80 230 90", name = "Green Slime", flash = "255 90 90", fx = "sigf/pslime_green.vmt" },
	purpleslime = { model = "models/props/metal_box_fx_fizzler.mdl", scale = 1.2, r = 48.0, hp = 44, col = "190 90 255", name = "Purple Slime", flash = "255 90 90", fx = "sigf/pslime_purple.vmt" },
	eye = { model = "models/npcs/personality_sphere_angry.mdl", scale = 1.8, r = 28.0, hp = 30, col = "255 255 255", name = "Demon Eye", flash = "255 255 80", fx = "sigf/p_blood.vmt" },
	king = { model = "models/props/metal_box_fx_fizzler.mdl", scale = 3.0, r = 100.0, hp = 380, col = "60 110 255", name = "King Slime", flash = "255 90 90", fx = "sigf/pslime_blue.vmt" },
	cthulhu = { model = "models/npcs/personality_sphere_angry.mdl", scale = 5.4, r = 80.0, hp = 400, col = "255 255 255", name = "Eye of Cthulhu", flash = "255 255 80", fx = "sigf/p_blood.vmt" }
}

::TTItems <- ["Stone Block", "Grass Block", "Dirt Block", "Sand Block", "Dynamite"]

// ---------------------------------------------------------------- small helpers

::TTFlat <- function(from, to) {
	local d = to - from
	d.z = 0.0
	local l = d.Length()
	if (l < 1.0) return Vector(0.0, -1.0, 0.0)
	return d * (1.0 / l)
}

::TTDist2 <- function(a, b) {
	local d = a - b
	d.z = 0.0
	return d.Length()
}

::TTClamp <- function(v, lo, hi) { return v < lo ? lo : (v > hi ? hi : v) }

// A burst of pixel particles: an env_smokestack that emits material `mat`, tinted by `col`.
::TTPuff <- function(pos, mat, col, size, amount, jet, speed, spread, dur = 0.15) {
	local sm = Entities.CreateByClassname("env_smokestack")
	if (sm == null) return
	sm.SetOrigin(pos)
	sm.__KeyValueFromInt("JetLength", jet)
	sm.__KeyValueFromInt("Rate", (amount / dur).tointeger() + 1)
	sm.__KeyValueFromInt("BaseSpread", spread)
	sm.__KeyValueFromInt("StartSize", size)
	sm.__KeyValueFromInt("EndSize", size)
	sm.__KeyValueFromInt("SpreadSpeed", spread)
	sm.__KeyValueFromInt("Speed", speed)
	sm.__KeyValueFromInt("twist", 0)
	sm.__KeyValueFromString("rendercolor", col)
	sm.__KeyValueFromInt("renderamt", 255)
	sm.__KeyValueFromString("SmokeMaterial", mat)
	EntFireByHandle(sm, "TurnOn", "", 0.02, null, null)
	EntFireByHandle(sm, "TurnOff", "", 0.02 + dur, null, null)
	EntFireByHandle(sm, "Kill", "", dur + 4.0, null, null)
}

// Terraria damage number (red, or yellow for a critical hit)
::TTNumber <- function(pos, dmg, crit) {
	local n = dmg
	if (n > 30) n = 30
	if (crit) {
		if (n < 10) n = 10
		::TTPuff(pos, "sigf/p_crit" + n + ".vmt", "255 255 255", 26, 1, 90, 55, 2, 0.05)
	} else {
		if (n < 5) n = 5
		::TTPuff(pos, "sigf/p_dmg" + n + ".vmt", "255 255 255", 20, 1, 80, 50, 2, 0.05)
	}
}

::TTShake <- function(amp, dur) {
	local s = Entities.CreateByClassname("env_shake")
	if (s == null) return
	s.SetOrigin(Vector(::TT.PX, ::TT.PY, ::TT.FZ + 60.0))
	s.__KeyValueFromFloat("amplitude", amp)
	s.__KeyValueFromFloat("duration", dur)
	s.__KeyValueFromFloat("frequency", 40.0)
	s.__KeyValueFromFloat("radius", 2000.0)
	s.__KeyValueFromInt("spawnflags", 5)
	EntFireByHandle(s, "StartShake", "", 0.02, null, null)
	EntFireByHandle(s, "Kill", "", dur + 1.0, null, null)
}

::TTFade <- function(col, amt, dur) {
	local f = Entities.CreateByClassname("env_fade")
	if (f == null) return
	f.__KeyValueFromString("rendercolor", col)
	f.__KeyValueFromInt("renderamt", amt)
	f.__KeyValueFromFloat("duration", dur)
	f.__KeyValueFromFloat("holdtime", 0.0)
	f.__KeyValueFromInt("spawnflags", 1)
	EntFireByHandle(f, "Fade", "", 0.02, null, null)
	EntFireByHandle(f, "Kill", "", dur + 1.0, null, null)
}

::TTBoom <- function(pos, mag) {
	local e = Entities.CreateByClassname("env_explosion")
	if (e == null) return
	e.SetOrigin(pos)
	e.__KeyValueFromInt("iMagnitude", mag)
	e.__KeyValueFromInt("spawnflags", 1 + 16)
	EntFireByHandle(e, "Explode", "", 0.02, null, null)
	EntFireByHandle(e, "Kill", "", 3.0, null, null)
}

// Terraria chat line (lower left), colored like the game's event messages.
::TTChat <- function(text, col = "255 255 255") {
	SigfText(text, 0.03, 0.66, 5.0, col, 1)
}

// ---------------------------------------------------------------- enemies

// kind: key of TTKinds; pos: floor point (x, y) where it appears.
::TTSpawn <- function(kind, pos) {
	local k = ::TTKinds[kind]
	local fly = (kind == "eye" || kind == "cthulhu")
	local f = {
		kind = kind, k = k, ent = null, extra = null, dead = false,
		hp = k.hp, maxhp = k.hp, r = k.r, fly = fly,
		pos = Vector(pos.x, pos.y, fly ? ::TT.FZ + 170.0 : ::TT.FZ + k.r),
		vel = Vector(0.0, 0.0, 0.0), v = Vector(0.0, 0.0, 0.0), air = false, nextHop = Time() + RandomFloat(0.3, 1.0),
		born = Time(), atk = Time() + 1.5, phase = 1, dash = 0.0, dashDir = Vector(0.0, -1.0, 0.0), nextDash = Time() + 3.0,
		nextMinion = Time() + 4.0, split = 0, bob = RandomFloat(0.0, 6.0), hitAt = {}, nextTouch = 0.0, spin = 0.0
	}
	::TT.foes.append(f)
	SigfDynamic(k.model, f.pos, 0, function(e):(f, k) {
		f.ent = e
		SigfScale(e, k.scale)
		SigfColor(e, k.col)
		e.SetAngles(0.0, 270.0, 0.0)
	})
	if (kind == "king") {
		SigfDynamic("models/player/items/ballbot/ballbot_crown.mdl", f.pos, 0, function(e):(f) {
			f.extra = e
			SigfScale(e, 4.0)
		})
	}
	return f
}

::TTAlive <- function() {
	local n = 0
	foreach (f in ::TT.foes) { if (!f.dead) n++ }
	return n
}

::TTBossAlive <- function() {
	foreach (f in ::TT.foes) { if (!f.dead && (f.kind == "king" || f.kind == "cthulhu")) return f }
	return null
}

::TTNearest <- function(from) {
	local best = null
	local bd = 99999.0
	foreach (f in ::TT.foes) {
		if (f.dead || f.ent == null) continue
		local d = (f.pos - from).Length()
		if (d < bd) { bd = d; best = f }
	}
	return best
}

::TTRestore <- function(f) {
	if (f.ent != null && f.ent.IsValid() && !f.dead) SigfColor(f.ent, f.k.col)
}

::TTHeartDrop <- function(pos) {
	::TTPuff(pos + Vector(0.0, 0.0, 30.0), "sigf/p_heart.vmt", "255 255 255", 22, 1, 120, 40, 4, 0.1)
	::TT.life = ::TTClamp(::TT.life + 20, 0, 400)
}

::TTKill <- function(f) {
	if (f.dead) return
	f.dead = true
	::TT.kills++
	local big = (f.kind == "king" || f.kind == "cthulhu")
	SigfSound(f.fly ? "sigf/enemy_hit.wav" : "sigf/slime_hit.wav")
	::TTPuff(f.pos, f.k.fx, "255 255 255", big ? 40 : 22, big ? 22 : 9, big ? 160 : 90, big ? 220 : 130, big ? 40 : 25, 0.2)
	::TTPuff(f.pos, "particle/SmokeStack.vmt", "90 90 90", big ? 80 : 34, big ? 12 : 4, big ? 100 : 60, 90, 25, 0.15)
	if (f.ent != null && f.ent.IsValid()) f.ent.Destroy()
	if (f.extra != null && f.extra.IsValid()) f.extra.Destroy()
	if (big) {
		::TTBoom(f.pos, 22)
		::TTShake(10.0, 1.2)
		SigfSound("sigf/pickup.wav")
		for (local i = 0; i < 6; i++) {
			local p = f.pos + Vector(RandomFloat(-90.0, 90.0), RandomFloat(-60.0, 60.0), RandomFloat(-20.0, 60.0))
			SigfIn(i * 0.15, function():(p) { ::TTHeartDrop(p) })
		}
		::TTChat(f.k.name + " has been defeated!", "255 220 60")
		::TT.life = 400
	} else {
		if (RandomInt(0, 2) == 0) ::TTHeartDrop(f.pos)
	}
}

// dmg dealt to f by something coming from `from`.
::TTHurt <- function(f, dmg, from, power) {
	if (f.dead) return
	local crit = RandomInt(0, 6) == 0
	if (crit) dmg = dmg * 2
	f.hp -= dmg
	::TTNumber(f.pos + Vector(RandomFloat(-10.0, 10.0), 0.0, f.r + 20.0), dmg, crit)
	local d = f.pos - from
	d.z = 0.0
	if (d.Length() < 1.0) d = Vector(0.0, 1.0, 0.0)
	d.Norm()
	local big = (f.kind == "king" || f.kind == "cthulhu")
	f.vel = f.vel + d * (big ? 40.0 : power)
	if (!f.fly && !f.air && !big) { f.air = true; f.vel.z = 160.0 }
	if (f.fly && !big) f.vel.z = f.vel.z + 40.0
	if (f.ent != null && f.ent.IsValid()) {
		SigfColor(f.ent, f.k.flash)
		SigfIn(0.14, function():(f) { ::TTRestore(f) })
	}
	::TTPuff(f.pos, f.k.fx, "255 255 255", big ? 26 : 15, big ? 8 : 5, 70, 110, 30, 0.1)
	SigfSound(f.fly ? "sigf/enemy_hit.wav" : "sigf/slime_hit.wav")
	if (f.hp <= 0) { ::TTKill(f); return }
	// King Slime sheds small slimes as it loses health
	if (f.kind == "king") {
		local lost = 1.0 - f.hp.tofloat() / f.maxhp.tofloat()
		local steps = (lost * 4.0).tointeger()
		while (f.split < steps) {
			f.split++
			::TTSpawn(f.split % 2 == 0 ? "slime" : "greenslime", Vector(f.pos.x + RandomFloat(-60.0, 60.0), f.pos.y - 40.0, 0.0))
		}
	}
	if (f.kind == "cthulhu" && f.phase == 1 && f.hp < f.maxhp / 2) {
		f.phase = 2
		SigfSound("sigf/boss_roar.wav")
		::TTShake(8.0, 1.5)
		::TTChat("Eye of Cthulhu is enraged!", "255 80 80")
		if (f.ent != null && f.ent.IsValid()) {
			SigfScale(f.ent, f.k.scale * 1.12)
			f.r = f.r * 1.12
			f.k = { model = f.k.model, scale = f.k.scale, r = f.k.r, hp = f.k.hp, col = "255 130 130", name = f.k.name, flash = f.k.flash, fx = f.k.fx }
			SigfColor(f.ent, "255 130 130")
		}
	}
}

// ---------------------------------------------------------------- the hero throws blocks

::TTThrowAt <- function(target, extra) {
	local host = SigfHost()
	local eye = host.EyePosition()
	// the view is locked down the walkway (+y)
	local fwd = Vector(0.0, 1.0, 0.0)
	local right = Vector(1.0, 0.0, 0.0)
	local from = eye + fwd * 170.0 + right * 62.0 + Vector(0.0, 0.0, -44.0)
	local T = ::TTClamp((target.pos - from).Length() / 760.0, 0.2, 0.8)
	local dest = target.pos + target.v * T
	local d = dest - from
	local v = Vector(d.x / T + RandomFloat(-12.0, 12.0) * extra, d.y / T, d.z / T + 600.0 * T * 0.5)
	local skin = RandomInt(0, 3)
	if (RandomInt(0, 5) == 0) skin = 4
	SigfProp("models/props/metal_box.mdl", from, 3.0, function(e):(v, skin) {
		EntFireByHandle(e, "Skin", skin.tostring(), 0.0, null, null)
		::TT.blocks.append({ e = e, p = e.GetOrigin(), v = 0.0, born = Time(), bomb = (skin == 4), portal = false })
		SigfPush(e, v)
	})
	::TT.item = ::TTItems[skin]
	::TT.itemAt = Time()
	SigfSound("sigf/throw.wav")
}

// Two real floor portals: blocks dropped into the one near the hero come out of the other,
// under the monster, going up at full speed.
::TTPortalShot <- function(target) {
	if (target == null || target.dead) return false
	local now = Time()
	if (now < ::TT.portalUntil) return false
	::TT.portalUntil = now + 8.0
	::TT.nextThrow = now + 3.2
	local a = Vector(::TT.PX + (RandomInt(0, 1) == 0 ? -80.0 : 80.0), ::TT.PY + 190.0, ::TT.FZ + 1.0)
	local b = Vector(::TTClamp(target.pos.x, ::TT.X0 + 60.0, ::TT.X1 - 60.0), target.pos.y, ::TT.FZ + 1.0)
	if (!target.fly) { target.nextHop = now + 3.2; target.vel = Vector(0.0, 0.0, 0.0) }
	local ang = Vector(-90.0, 0.0, 0.0)
	local la = format("%.1f %.1f %.1f -90 0 0", a.x, a.y, a.z)
	local lb = format("%.1f %.1f %.1f -90 0 0", b.x, b.y, b.z)
	SigfSpawn("prop_portal", a, { LinkageGroupID = 7, PortalTwo = 0 }, 7.0, null, ang, [["SetActivatedState", "1"], ["NewLocation", la]])
	SigfSpawn("prop_portal", b, { LinkageGroupID = 7, PortalTwo = 1 }, 7.0, null, ang, [["SetActivatedState", "1"], ["NewLocation", lb]])
	::TT.rps.append({ a = a, b = b, die = now + 7.0 })
	::TTChat("Blocks fall through the portal!", "120 200 255")
	for (local i = 0; i < 4; i++) {
		SigfIn(1.0 + i * 0.55, function():(a, i) {
			local p = a + Vector(RandomFloat(-18.0, 18.0), RandomFloat(-18.0, 18.0), 150.0)
			local skin = i == 2 ? 4 : RandomInt(0, 3)
			SigfProp("models/props/metal_box.mdl", p, 4.0, function(e):(skin) {
				EntFireByHandle(e, "Skin", skin.tostring(), 0.0, null, null)
				::TT.blocks.append({ e = e, p = e.GetOrigin(), v = 0.0, born = Time(), bomb = (skin == 4), portal = true })
			})
		})
	}
	return true
}

// ---------------------------------------------------------------- per-frame logic

::TTMoveSlime <- function(f, dt, now, host) {
	local tgt = Vector(::TT.PX, ::TT.PY + 270.0, 0.0)
	local big = f.kind == "king"
	local ground = ::TT.FZ + f.r
	if (f.air) {
		f.vel.z = f.vel.z - ::TT.G * dt
		f.pos = f.pos + f.vel * dt
		f.spin += dt * (big ? 40.0 : 140.0)
		if (f.pos.z <= ground && f.vel.z < 0.0) {
			f.pos.z = ground
			f.air = false
			f.vel.x = 0.0
			f.vel.y = 0.0
			f.vel.z = 0.0
			f.nextHop = now + (big ? RandomFloat(1.0, 1.6) : RandomFloat(0.5, 1.2))
			if (big) { ::TTShake(5.0, 0.5); ::TTPuff(Vector(f.pos.x, f.pos.y, ::TT.FZ + 8.0), "particle/SmokeStack.vmt", "170 170 170", 50, 10, 60, 160, 30, 0.12) }
		}
	} else {
		f.pos.z = ground
		if (f.vel.Length() > 4.0) {
			f.pos = f.pos + f.vel * dt
			f.vel = f.vel * ::TTClamp(1.0 - 7.0 * dt, 0.0, 1.0)
		}
		if (now >= f.nextHop) {
			local dir = ::TTFlat(f.pos, tgt)
			local sp = big ? 150.0 : 125.0
			f.vel = Vector(dir.x * sp + RandomFloat(-25.0, 25.0), dir.y * sp, big ? 400.0 : 330.0)
			f.air = true
		}
	}
	f.pos.x = ::TTClamp(f.pos.x, ::TT.X0 + f.r * 0.6, ::TT.X1 - f.r * 0.6)
	if (f.pos.y < ::TT.PY + 220.0) f.pos.y = ::TT.PY + 220.0
}

::TTMoveEye <- function(f, dt, now, host) {
	local eyep = host.EyePosition()
	f.bob += dt * 3.0
	if (f.kind == "cthulhu") {
		if (f.dash > 0.0) {
			// charging at the hero
			f.pos = f.pos + f.dashDir * ((f.phase == 2 ? 520.0 : 420.0) * dt)
			f.dash -= dt
			if (f.dash <= 0.0) f.nextDash = now + (f.phase == 2 ? 1.8 : 3.2)
		} else {
			// hover around the hero, then wind up a dash
			local ang = now * (f.phase == 2 ? 1.3 : 0.8)
			local want = Vector(::TT.PX + cos(ang) * 110.0, ::TT.PY + 370.0 + sin(ang) * 50.0, ::TT.FZ + 125.0 + sin(f.bob) * 25.0)
			local d = want - f.pos
			f.pos = f.pos + d * ::TTClamp(2.2 * dt, 0.0, 1.0)
			if (now >= f.nextDash) {
				f.dash = 0.7
				f.dashDir = eyep - f.pos
				f.dashDir.Norm()
				f.dashDir.z = f.dashDir.z * 0.4
				SigfSound("sigf/boss_roar.wav")
			}
		}
		if (f.phase == 2 && now >= f.nextMinion) {
			f.nextMinion = now + 5.0
			::TTSpawn("eye", Vector(f.pos.x + RandomFloat(-60.0, 60.0), f.pos.y, 0.0))
		}
	} else {
		// demon eye: drifts at the hero with a wobble
		local want = Vector(eyep.x + sin(f.bob * 0.7) * 70.0, eyep.y + 240.0, ::TT.FZ + 110.0 + sin(f.bob) * 40.0)
		local d = want - f.pos
		local l = d.Length()
		if (l > 1.0) f.pos = f.pos + d * (1.0 / l) * (150.0 * dt)
	}
	// knockback
	f.pos = f.pos + f.vel * dt
	f.vel = f.vel * ::TTClamp(1.0 - 4.0 * dt, 0.0, 1.0)
	f.pos.x = ::TTClamp(f.pos.x, ::TT.X0 - 40.0, ::TT.X1 + 40.0)
	if (f.pos.z < ::TT.FZ + f.r) f.pos.z = ::TT.FZ + f.r
}

// the new monsters use the portals too: whoever stands in one comes out of the other
::TTPortals <- function(f, now) {
	if (f.fly) return
	foreach (o in ::TT.rps) {
		if (o.die < now) continue
		local pairs = [[o.a, o.b], [o.b, o.a]]
		foreach (p in pairs) {
			if (::TTDist2(f.pos, p[0]) < 46.0 && f.pos.z < ::TT.FZ + f.r + 40.0 && now > f.nextTouch) {
				f.nextTouch = now + 2.0
				f.pos = Vector(p[1].x, p[1].y, ::TT.FZ + f.r + 20.0)
				f.vel = Vector(RandomFloat(-60.0, 60.0), 40.0, 460.0)
				f.air = true
				::TTPuff(p[1] + Vector(0.0, 0.0, 30.0), "sigf/p_star.vmt", "255 255 255", 18, 5, 90, 90, 20, 0.1)
				return
			}
		}
	}
}

::TTHits <- function(f, now) {
	foreach (b in ::TT.blocks) {
		if (f.dead) return
		if (b.v < 230.0 || !b.e.IsValid()) continue
		local bp = b.p
		if ((bp - f.pos).Length() > f.r + 34.0) continue
		local id = b.e.entindex()
		if ((id in f.hitAt) && f.hitAt[id] > now - 0.5) continue
		f.hitAt[id] <- now
		local dmg = RandomInt(8, 15)
		if (b.v > 700.0) dmg += 4
		if (b.portal) dmg += 6
		::TTHurt(f, dmg, bp, 190.0)
		if (b.bomb) {
			// dynamite: blows up and hurts everything close by
			b.bomb = false
			::TTBoom(bp, 10)
			::TTShake(4.0, 0.4)
			::TTPuff(bp, "particle/SmokeStack.vmt", "255 140 40", 60, 14, 80, 160, 30, 0.12)
			foreach (o in ::TT.foes) {
				if (!o.dead && o != f && (o.pos - bp).Length() < 190.0) ::TTHurt(o, RandomInt(10, 16), bp, 220.0)
			}
		} else {
			::TTPuff(bp, "particle/SmokeStack.vmt", "150 100 60", 16, 6, 50, 100, 25, 0.1)
			SigfSound("sigf/block_break.wav")
		}
		b.e.Destroy()
		b.v = 0.0
	}
	foreach (cid, c in ::TT.cubes) {
		if (c.v < 260.0 || (c.p - f.pos).Length() > f.r + 40.0) continue
		if ((cid in f.hitAt) && f.hitAt[cid] > now - 0.6) continue
		f.hitAt[cid] <- now
		::TTHurt(f, RandomInt(14, 22), c.p, 230.0)
		if (f.dead) return
	}
}

// speeds of everything that can be thrown at the enemies
::TTTrack <- function(now, dt) {
	local keep = []
	foreach (b in ::TT.blocks) {
		if (!b.e.IsValid()) continue
		local p = b.e.GetOrigin()
		b.v = (p - b.p).Length() / dt
		b.p = p
		if (b.v < 25.0 && now - b.born > 0.8) {
			// a block that stopped crumbles away
			::TTPuff(p, "particle/SmokeStack.vmt", "150 100 60", 14, 4, 40, 80, 20, 0.08)
			b.e.Destroy()
			continue
		}
		keep.append(b)
	}
	::TT.blocks = keep
	// the chamber's own cubes hurt too when someone throws them
	for (local c = Entities.FindByClassname(null, "prop_weighted_cube"); c != null; c = Entities.FindByClassname(c, "prop_weighted_cube")) {
		local cid = c.entindex()
		local cp = c.GetOrigin()
		if (cid in ::TT.cubes) {
			local r = ::TT.cubes[cid]
			r.v = (cp - r.p).Length() / dt
			r.p = cp
		} else ::TT.cubes[cid] <- { p = cp, v = 0.0 }
	}
	local rp = []
	foreach (o in ::TT.rps) { if (o.die > now) rp.append(o) }
	::TT.rps = rp
}

::TTContact <- function(f, now, host) {
	if (now < f.atk) return
	if (f.pos.y - ::TT.PY > f.r * 0.4 + 285.0) return
	f.atk = now + (f.kind == "king" || f.kind == "cthulhu" ? 2.2 : 1.8)
	local dmg = f.kind == "king" ? 40 : (f.kind == "cthulhu" ? 36 : 12)
	::TT.life = ::TTClamp(::TT.life - dmg, 120, 400)
	::TT.hurtAt = now
	::TTFade("255 20 20", 60, 0.2)
	::TTPuff(host.EyePosition() + Vector(0.0, 220.0, 0.0) + Vector(-60.0, 0.0, -50.0), "sigf/p_dmg" + ::TTClamp(dmg, 12, 30) + ".vmt", "255 255 255", 16, 1, 70, 40, 2, 0.05)
	::TTShake(3.0, 0.3)
	SigfSound("sigf/enemy_hit.wav")
	// the enemy bounces back
	f.vel = f.vel + Vector(RandomFloat(-30.0, 30.0), 260.0, 0.0)
	if (!f.fly) { f.air = true; f.vel.z = 220.0 }
	if (f.fly && f.dash > 0.0) { f.dash = 0.0; f.nextDash = now + 2.5 }
}

::TTTick <- function() {
	if (!::SigfLive) return
	local now = Time()
	local dt = now - ::TT.last
	::TT.last = now
	if (dt <= 0.0) return
	if (dt > 0.1) dt = 0.1
	local host = SigfHost()
	if (host == null) return
	if (!::TT.ready) { ::TT.ready = true; ::TT.t0 = now }
	if (!::TT.dressed) { ::TT.dressed = true; ::TTDress() }
	if (!::SigfDemoMode && now >= ::TT.music) { ::TT.music = now + 62.0; SigfSound("sigf/theme.wav") }
	::TTTrack(now, dt)
	local keep = []
	foreach (f in ::TT.foes) {
		if (f.dead) continue
		keep.append(f)
		if (f.ent == null || !f.ent.IsValid()) continue
		local before = f.pos
		if (f.fly) ::TTMoveEye(f, dt, now, host)
		else ::TTMoveSlime(f, dt, now, host)
		f.v = (f.pos - before) * (1.0 / dt)
		::TTPortals(f, now)
		::TTHits(f, now)
		if (f.dead) continue
		::TTContact(f, now, host)
		f.ent.SetOrigin(f.pos)
		// face the hero (slimes also tumble while they hop)
		local fd = ::TTFlat(f.pos, host.GetOrigin())
		local yaw = atan2(fd.y, fd.x) * 57.29578
		if (f.fly) f.ent.SetAngles(0.0, yaw, 0.0)
		else if (f.kind == "king") f.ent.SetAngles(0.0, 270.0, 0.0)
		else f.ent.SetAngles(f.air ? sin(f.spin * 0.05) * 18.0 : 0.0, 270.0, f.air ? cos(f.spin * 0.05) * 14.0 : 0.0)
		if (f.extra != null && f.extra.IsValid()) f.extra.SetOrigin(f.pos + Vector(30.0, 0.0, f.r * 0.2))
	}
	::TT.foes = keep
	// the hero throws blocks at the closest enemy
	if (now >= ::TT.nextThrow && keep.len() > 0) {
		local tgt = ::TTNearest(host.GetOrigin())
		local bossF = ::TTBossAlive()
		// with a boss around, most blocks fly at the boss
		if (bossF != null && bossF.ent != null && RandomInt(0, 3) != 0) tgt = bossF
		if (tgt != null && tgt.ent != null) {
			local boss = (tgt.kind == "king" || tgt.kind == "cthulhu")
			::TTThrowAt(tgt, boss ? 0.5 : 1.5)
			::TT.nextThrow = now + (boss ? 0.45 : 0.8)
		}
	}
	// keep the camera on the walkway
	if (now - ::TT.look > 4.0) { ::TT.look = now; if (!::SigfDemoMode) SigfLook(3.0, 90.0) }
	if (now >= ::TT.hudAt) {
		::TT.hudAt = now + 0.9
		if (now - ::TT.hurtAt > 3.0) ::TT.life = ::TTClamp(::TT.life + 15, 0, 400)
		::TTHud(now)
	}
	if (!::SigfDemoMode && ::TT.director) ::TTDirector(now)
}

// Terraria-style HUD text: life and mana top right, item name top left, boss bar at the bottom
::TTHud <- function(now) {
	SigfText("Life: " + ::TT.life + "/400", 0.60, 0.03, 1.2, "255 60 60", 2)
	SigfText("Mana: 20/20", 0.60, 0.085, 1.2, "90 150 255", 0)
	if (now - ::TT.itemAt < 2.5) SigfText(::TT.item, 0.03, 0.05, 1.2, "255 255 255", 4)
	local b = ::TTBossAlive()
	if (b != null) {
		local n = (20.0 * b.hp / b.maxhp.tofloat()).tointeger()
		if (n < 1) n = 1
		local bar = ""
		for (local i = 0; i < 20; i++) bar += (i < n ? "|" : ".")
		SigfText(b.k.name + "\n" + bar, -1.0, 0.86, 1.2, b.kind == "king" ? "120 160 255" : "255 90 90", 3)
	}
}

// ---------------------------------------------------------------- the idle show (the demo drives its own)

::TTBoss <- function(kind) {
	if (kind == "king") {
		::TTChat("King Slime has awoken!", "200 120 255")
		SigfSound("sigf/boss_roar.wav")
		::TTSpawn("king", Vector(::TT.PX, ::TT.FARY, 0.0))
	} else {
		::TTChat("You feel an evil presence watching you...", "200 120 255")
		SigfSound("sigf/boss_roar.wav")
		::TTSpawn("cthulhu", Vector(::TT.PX, ::TT.FARY, 0.0))
	}
}

::TTDirector <- function(now) {
	if (now < ::TT.nextStep) return
	local alive = ::TTAlive()
	local x = RandomFloat(::TT.X0 + 50.0, ::TT.X1 - 50.0)
	local s = ::TT.step % 12
	if (s == 5 || s == 11) {
		if (::TTBossAlive() != null || alive > 0) { ::TT.nextStep = now + 2.0; return }
		::TTBoss(s == 5 ? "king" : "cthulhu")
		::TT.step++
		::TT.nextStep = now + 8.0
		return
	}
	if (alive > 3) { ::TT.nextStep = now + 2.0; return }
	if (::TTBossAlive() != null) { ::TT.nextStep = now + 4.0; return }
	local pick = ["slime", "greenslime", "eye", "purpleslime", "slime", "eye"][RandomInt(0, 5)]
	::TTSpawn(pick, Vector(x, ::TT.FARY, 0.0))
	::TT.step++
	::TT.nextStep = now + 3.5
	if (RandomInt(0, 6) == 0) {
		local tgt = ::TTNearest(Vector(::TT.PX, ::TT.PY, ::TT.FZ))
		if (tgt != null && !tgt.fly) ::TTPortalShot(tgt)
	}
}


// torch pillars along the walkway: two stacked blocks, a pixel flame on top
::TTFlame <- function(pos) {
	local sm = Entities.CreateByClassname("env_smokestack")
	if (sm == null) return
	sm.SetOrigin(pos)
	sm.__KeyValueFromInt("JetLength", 34)
	sm.__KeyValueFromInt("Rate", 9)
	sm.__KeyValueFromInt("BaseSpread", 3)
	sm.__KeyValueFromInt("StartSize", 17)
	sm.__KeyValueFromInt("EndSize", 11)
	sm.__KeyValueFromInt("SpreadSpeed", 3)
	sm.__KeyValueFromInt("Speed", 32)
	sm.__KeyValueFromInt("twist", 0)
	sm.__KeyValueFromString("rendercolor", "255 255 255")
	sm.__KeyValueFromInt("renderamt", 255)
	sm.__KeyValueFromString("SmokeMaterial", "sigf/p_flame.vmt")
	EntFireByHandle(sm, "TurnOn", "", 0.05, null, null)
}

::TTDress <- function() {
	local xs = [-226.0, 102.0]
	local ys = [-1195.0, -1095.0]
	foreach (x in xs) {
		foreach (y in ys) {
			local px = x
			local py = y
			SigfProp("models/props/metal_box.mdl", Vector(px, py, ::TT.FZ + 34.0), 0, function(e) {
				EntFireByHandle(e, "Skin", "2", 0.0, null, null)
				EntFireByHandle(e, "DisableMotion", "", 0.3, null, null)
			})
			SigfProp("models/props/metal_box.mdl", Vector(px, py, ::TT.FZ + 100.0), 0, function(e) {
				EntFireByHandle(e, "Skin", "3", 0.0, null, null)
				EntFireByHandle(e, "DisableMotion", "", 0.3, null, null)
			})
			::TTFlame(Vector(px, py, ::TT.FZ + 138.0))
		}
	}
}

// ---------------------------------------------------------------- set up

::TTStartClock <- function() {
	local old = Entities.FindByName(null, "tt_clock")
	if (old != null) old.Destroy()
	local t = Entities.CreateByClassname("logic_timer")
	t.__KeyValueFromString("targetname", "tt_clock")
	t.ValidateScriptScope()
	t.GetScriptScope().TTOnTimer <- function() { ::TTTick() }
	t.ConnectOutput("OnTimer", "TTOnTimer")
	EntFireByHandle(t, "RefireTime", "0.033", 0.0, null, null)
	EntFireByHandle(t, "Enable", "", 0.0, null, null)
}

SigfAfter(0.5, function() {
	SigfCmd("gameinstructor_enable 0")
	SigfCmd("fov_desired 64")
	SigfCmd("mat_autoexposure_min 1.7")
	SigfCmd("mat_autoexposure_max 1.7")
	::TTStartClock()
})
