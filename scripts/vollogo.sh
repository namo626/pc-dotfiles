vol=$(pamixer --get-mute)
case $vol in
	"true") bar=;;
	*) bar= ;;
esac

echo $bar


