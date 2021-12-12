<script setup lang="ts">
import "./assets/index.css";
import { Elm } from "./src/Main.elm";
import elmBridge from 'elm-vue-bridge'

const HelloWorld = elmBridge(Elm, "HelloWorld");
</script>

<template>
  <div>
    <HelloWorld />
  </div>
</template>
