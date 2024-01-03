class Rgbds < Formula
  desc "Rednex GameBoy Development System"
  homepage "https://rgbds.gbdev.io"
  url "https://github.com/gbdev/rgbds/archive/refs/tags/v0.7.0.tar.gz"
  sha256 "ef04d24d7a79c05ffadac0c08214f59b8d8812c7d1052a585e5ab0145f093b30"
  license "MIT"
  head "https://github.com/gbdev/rgbds.git", branch: "master"

  livecheck do
    url :stable
    strategy :github_latest
  end

  depends_on "bison" => :build
  depends_on "cmake" => :build
  depends_on "pkg-config" => :build
  depends_on "libpng"

  def install
    system "cmake", "-S", ".", "-B", "build",
           "-DCMAKE_C_FLAGS=-O3 -flto -DNDEBUG -mmacosx-version-min=10.9",
           "-DCMAKE_CXX_FLAGS=-O3 -flto -DNDEBUG -mmacosx-version-min=10.9",
           *std_cmake_args

    system "cmake", "--build", "build"
    system "cmake", "--install", "build"
  end
end
