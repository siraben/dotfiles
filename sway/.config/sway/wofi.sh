#!/usr/bin/env sh

swaymsg -t get_tree |
  jq -r '.nodes[].nodes[] | if .nodes then [recurse(.nodes[])] else [] end + .floating_nodes | .[] | select(.nodes==[]) | ((.id | tostring) + "  "  +
.name)' |
  wofi -M fuzzy --show dmenu | {
    read -r id name
    swaymsg "[con_id=$id]" focus
}
