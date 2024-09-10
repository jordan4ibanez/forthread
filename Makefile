default:
	@fpm run


windows:
	@fpm run --c-flag -Wno-discarded-qualifiers --c-flag -Wno-incompatible-pointer-types


gdb:
	@MALLOC_CHECK_=2 fpm run --flag   -g --flag   -lmcheck \
	                         --c-flag -g --c-flag -lmcheck

valgrind:
	@./scripts/create_version_info.sh
	@valgrind --trace-children=yes --leak-check=full fpm run --flag   -g \
	                                                         --c-flag -g


release:
	@fpm run --flag   -fuse-ld=mold --flag   -O3 --flag   -march=native --flag   -mtune=native --flag   -g \
	         --c-flag -fuse-ld=mold --c-flag -O3 --c-flag -march=native --c-flag -mtune=native --c-flag -g


mac-release:
	@fpm run --flag   -O3 --flag   -march=native --flag   -mtune=native --flag   -g \
	         --c-flag -O3 --c-flag -march=native --c-flag -mtune=native --c-flag -g


.PHONY: test
test:
	@fpm test


.PHONY: test_gdb
test_gdb:
	@MALLOC_CHECK_=2 fpm test --flag   -g --flag   -lmcheck \
	                          --c-flag -g --c-flag -lmcheck


.PHONY: test_valgrind
test_valgrind:
	@valgrind --trace-children=yes --leak-check=full fpm test --flag   -g \
	                                                          --c-flag -g


#! CLEANING COMMANDS.
	
# Use this if the vscode extension gives up.
clean:
	@./scripts/clear_mod_files.sh
	@./scripts/remove_build_folder.sh

