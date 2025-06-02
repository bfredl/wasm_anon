(module $nle.wasm
  (func (export "loop")  (param $lim i32) (result i32)
    (local $i i32)
    (local $acc i32)
      i32.const 0
      local.set $i ;; i
      i32.const 0
      local.set $acc ;; acc

      loop ;;
        ;; acc = acc + i*i;
        local.get $i
        local.get $i
        i32.mul
        local.get $acc
        i32.add
        local.set $acc

        local.get $i

        ;; i = i + 1 ;
        local.get $i ;; i
        i32.const $i
        i32.add
        local.tee $i

        i32.const 5
        i32.eq
        if
          i32.const 10
          local.set $i
        end

        ;; if (i < lim) continue;
        local.get $i
        local.get $lim
        i32.lt
        br_if 0
      end
      local.get 2)
  )

