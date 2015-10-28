read -p "Specify Debug or Release build: "  config
if [ $config = Debug ] || [ $config = Release ]
    then echo "Building $config build"
    xbuild ./Experiments.fsproj /property:Configuration=$config
    echo "$config build script terminated"
fi
