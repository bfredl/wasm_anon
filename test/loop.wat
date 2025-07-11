(module $loop.wasm
  (func (export "loop") (param $lim i32) (result i32)
    (local $i i32) (local $acc i32)
      i32.const 0
      local.set $i
      i32.const 0
      local.set $acc

      loop ;;
        ;; acc = acc + i*i;
        local.get $i
        local.get $i
        i32.mul
        local.get $acc
        i32.add
        local.set $acc

        ;; i = i + 1 ;
        local.get $i ;; i
        i32.const 1
        i32.add
        local.tee $i

        ;; if (i != param_0) continue;
        local.get $lim
        i32.ne
        br_if 0
      end
      local.get $acc)
)
