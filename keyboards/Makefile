
.PHONY: eyelash_corne
eyelash_corne: firmwares/eyelash_corne_left.uf2 firmwares/eyelash_corne_right.uf2

firmwares/eyelash_corne_left.uf2: eyelash_corne/config/* eyelash_corne/Dockerfile
	docker build --target output_collector --output type=local,dest="firmwares" -f eyelash_corne/Dockerfile eyelash_corne
	touch firmwares/eyelash_corne_*.uf2

firmwares/eyelash_corne_right.uf2: eyelash_corne/config/* eyelash_corne/Dockerfile
	docker build --target output_collector --output type=local,dest="firmwares" -f eyelash_corne/Dockerfile eyelash_corne
	touch firmwares/eyelash_corne_*.uf2
