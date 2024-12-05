(module $loop.wasm
  (type (;0;) (func (param i32) (result i32)))
  (func $loop (type 0) (param i32) (result i32)
    (local i32 i32)
      i32.const 0
      local.set 1 ;; i
      i32.const 0
      local.set 2 ;; acc

      loop ;;
        ;; acc = acc + i*i;
        local.get 1 ;; i
        local.get 1 ;; i
        i32.mul
        local.get 2
        i32.add
        local.set 2

        ;; i = i + 1 ;
        local.get 1 ;; i
        i32.const 1
        i32.add
        local.tee 1

        ;; if (i != param_0) continue;
        local.get 0
        i32.ne
        br_if 0
      end
      local.get 2)
  (export "loop" (func $loop)))
