// Terraria Takeover demo: one monster after the other, each with a short caption.
// Every call below is a step of the show; the mod itself (sigf_mod.nut) does the fighting.

::DemoNearGround <- function() {
	local best = null
	local bd = 99999.0
	foreach (f in ::TT.foes) {
		if (f.dead || f.fly || f.ent == null) continue
		local d = f.pos.y - ::TT.PY
		if (d < bd) { bd = d; best = f }
	}
	return best
}

SigfDemo(0.2, function() {
	SigfLook(3.0, 90.0)
	::TT.look = Time()
	::TTSpawn("slime", Vector(-130.0, ::TT.FARY - 90.0, 0.0))
	::TTSpawn("greenslime", Vector(10.0, ::TT.FARY - 130.0, 0.0))
})
SigfDemo(1.0, function() { SigfCaption("TERRARIA TAKEOVER", 4); SigfSound("sigf/theme.wav") })
SigfDemo(4.0, function() {
	SigfCaption("Slimes and Demon Eyes invade the chamber", 4)
	::TTSpawn("eye", Vector(-60.0, ::TT.FARY, 0.0))
	::TTSpawn("purpleslime", Vector(-20.0, ::TT.FARY - 30.0, 0.0))
})
SigfDemo(7.0, function() {
	::TTSpawn("purpleslime", Vector(-100.0, ::TT.FARY - 120.0, 0.0))
	::TTSpawn("slime", Vector(10.0, ::TT.FARY - 140.0, 0.0))
})
SigfDemo(8.6, function() {
	SigfCaption("Portal shot: blocks fall in, come out under it", 5)
	local t = ::DemoNearGround()
	if (t == null) t = ::TTNearest(Vector(::TT.PX, ::TT.PY, ::TT.FZ))
	::TTPortalShot(t)
})
SigfDemo(12.5, function() {
	SigfCaption("KING SLIME awakens!", 4)
	::TTBoss("king")
})
SigfDemo(19.0, function() {
	SigfCaption("Portal + dynamite hits the boss", 4)
	::TTPortalShot(::TTBossAlive())
})
SigfDemo(28.5, function() {
	if (::TTBossAlive() != null) { local b = ::TTBossAlive(); ::TTHurt(b, 900, b.pos, 0.0) }
})
SigfDemo(29.5, function() {
	SigfCaption("EYE OF CTHULHU", 4)
	::TTBoss("cthulhu")
})
SigfDemo(38.0, function() {
	::TTPortalShot(::TTBossAlive())
})
SigfDemo(49.0, function() {
	if (::TTBossAlive() != null) { local b = ::TTBossAlive(); ::TTHurt(b, 900, b.pos, 0.0) }
})
SigfDemo(50.5, function() {
	SigfCaption("Slime rain!", 3)
	::TTSpawn("slime", Vector(-150.0, ::TT.FARY, 0.0))
	::TTSpawn("greenslime", Vector(-40.0, ::TT.FARY - 20.0, 0.0))
	::TTSpawn("purpleslime", Vector(30.0, ::TT.FARY, 0.0))
	SigfIn(1.2, function() {
		::TTSpawn("slime", Vector(-90.0, ::TT.FARY, 0.0))
		::TTSpawn("greenslime", Vector(20.0, ::TT.FARY - 30.0, 0.0))
	})
})
SigfDemo(56.0, function() {
	local t = ::DemoNearGround()
	if (t != null) ::TTPortalShot(t)
})
SigfDemo(60.0, function() {
	::TTSpawn("eye", Vector(-100.0, ::TT.FARY, 0.0))
	::TTSpawn("eye", Vector(0.0, ::TT.FARY, 0.0))
	::TTSpawn("slime", Vector(40.0, ::TT.FARY - 20.0, 0.0))
})
SigfDemo(66.0, function() {
	SigfCaption("Terraria meets Portal 2", 6)
	::TTSpawn("purpleslime", Vector(-100.0, ::TT.FARY, 0.0))
	::TTSpawn("eye", Vector(0.0, ::TT.FARY, 0.0))
})
