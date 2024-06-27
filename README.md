# genit
Genit is a Nim macro(s) works like an inline template. It implements a DSL to help you produce lots of repetitive code.
This is an experiment to see if we eliminate copy-paste errors and increase maintainablity at the cost of a little clarity.

---

### Documentation
https://geekrelief.github.io/genit/

---
### Examples
---

Basic example with unnamed arguments:
```nim
gen red, green, blue:
  const `it tag` = TM_STATIC_HASH("color_" & $$it)
```

Produces:
``` nim
const redtag = TM_STATIC_HASH("color_" & "red")
const greentag = TM_STATIC_HASH("color_" & "green")
const bluetag = TM_STATIC_HASH("color_" & "blue")
```

---

Here we can nest `gen` calls:
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
  s.movercomponent = entity_api.lookupcomponenttype(s.entity_ctx, TM_TT_TYPE_HASH_PHYSX_MOVERcomponent)
  s.physics_jointcomponent = entity_api.lookupcomponenttype(s.entity_ctx, TM_TT_TYPE_HASH physics_jointcomponent)
  s.physics_shapecomponent = entity_api.lookupcomponenttype(s.entity_ctx, TM_TT_TYPE_HASHphysics_shapecomponent)
  s.physx_jointcomponent = entity_api.lookupcomponenttype(s.entity_ctx, TM_TT_TYPE_HASHphysx_jointcomponent)
  s.physx_rigid_bodycomponent = entity_api.lookupcomponenttype(s.entity_ctx, TM_TT_TYPE_HASH physx_rigid_bodycomponent)
  s.tagcomponent = entity_api.lookupcomponenttype(s.entity_ctx, TM_TT_TYPE_HASHtagcomponent)
  s.transformcomponent = entity_api.lookupcomponenttype(s.entity_ctx, TM_TT_TYPE_HASHtransformcomponent)

  s.transman = cast[ptr tmtransformcomponentmanager_o](entity_api.componentmanager(s.entity_ctx, s.transformcomponent))
  s.tagman = cast[ptr tmtagcomponentmanager_o](entity_api.componentmanager(s.entity_ctx, s.tagcomponent))
```
---

Using operations in backticks:
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

---

Wrap them in templates for macro-like power:
```nim
template genTransceiverComponent(name, typ: untyped): untyped {.dirty.} =
  gen (name, typ):
    CreateTransceiverComponent(`U it[0] SignalTransceiver`, `FOn it[0] SignalDelegate`, it[1], $$it[0])

genTransceiverComponent(Name, FName)
genTransceiverComponent(String, FString)
genTransceiverComponent(Integer, int)
genTransceiverComponent(Float, float)
genTransceiverComponent(Vector, FVector)
```

Produces:
```nim
CreateTranscieverComponent(UNameSignalTransceiver, FOnNameSignalDelegate, FName, "Name")
CreateTranscieverComponent(UStringSignalTransceiver, FOnStringSignalDelegate, FString, "String")
CreateTranscieverComponent(UIntegerSignalTransceiver, FOnIntegerSignalDelegate, int, "Integer")
CreateTranscieverComponent(UFloatSignalTransceiver, FOnFloatSignalDelegate, float, "Float")
CreateTranscieverComponent(UVectorSignalTransceiver, FOnVectorSignalDelegate, FVector, "Vector")
```