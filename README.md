# genit
The `gen` macro implements a small DSL that works like an inline proc but for templates which can be used to inline, repetitive code. 

---

### Documentation
https://geekrelief.github.io/genit/

---
### Examples
---
```nim
gen red, green, blue:
  const `it tag` = TM_STATIC_HASH("color_" & $$it)
```

Produces:
``` nim
const `red tag` = TM_STATIC_HASH("color_" & "red")
const `green tag` = TM_STATIC_HASH("color_" & "green")
const `blue tag` = TM_STATIC_HASH("color_" & "blue")
```

---
```nim
gen(c = component):
  s.`mover c` = entity_api.`lookup c type`(s.entity_ctx, `TM_TT_TYPE_HASH_PHYSX_MOVER c`)
  gen physics_joint, physics_shape, physx_joint, physx_rigid_body, tag, transform:
    s.`it c` = entity_api.`lookup c type`(s.entity_ctx, `TM_TT_TYPE_HASH it c`)

  gen(m = manager, (trans, transform), (tag, tag)):
    s.`it[0] man` = cast[ptr `tm it[1] c m o`](entity_api.`c m`(s.entity_ctx, s.`it[1] c`))
```

Produces:

```nim
  s.`mover component` = entity_api.`lookup component type`(s.entity_ctx, `TM_TT_TYPE_HASH_PHYSX_MOVER component`)
  s.`physics_joint component` = entity_api.`lookup component type`(s.entity_ctx, `TM_TT_TYPE_HASH physics_joint component`)
  s.`physics_shape component` = entity_api.`lookup component type`(s.entity_ctx, `TM_TT_TYPE_HASH physics_shape component`)
  s.`physx_joint component` = entity_api.`lookup component type`(s.entity_ctx, `TM_TT_TYPE_HASH physx_joint component`)
  s.`physx_rigid_body component` = entity_api.`lookup component type`(s.entity_ctx, `TM_TT_TYPE_HASH physx_rigid_body component`)
  s.`tag component` = entity_api.`lookup component type`(s.entity_ctx, `TM_TT_TYPE_HASH tag component`)
  s.`transform component` = entity_api.`lookup component type`(s.entity_ctx, `TM_TT_TYPE_HASH transform component`)

  s.`trans man` = cast[ptr `tm transform component manager_o`](entity_api.`component manager`(s.entity_ctx, s.`transform component`))
  s.`tag man` = cast[ptr `tm tag component manager_o`](entity_api.`component manager`(s.entity_ctx, s.`tag component`))
```
---

```nim
  gen (A, `-=`, x), (D, `+=`, x), (W, `-=`, z), (S, `+=`, z):
    if s.input.held_keys[`TM_INPUT_KEYBOARD_ITEM it[0]`]:
      it[1](local_movement.`it[2]`, 1.0f)
```

Produces:
```nim
  if s.input.held_keys[TM_INPUT_KEYBOARD_ITEM_A]:
    `-=`(local_movement.x, 1.0f)
  if s.input.held_keys[TM_INPUT_KEYBOARD_ITEM_D]:
    `+=`(local_movement.x, 1.0f)
  if s.input.held_keys[TM_INPUT_KEYBOARD_ITEM_W]:
    `-=`(local_movement.z, 1.0f)
  if s.input.held_keys[TM_INPUT_KEYBOARD_ITEM_S]:
    `+=`(local_movement.z, 1.0f)
```