plugins {
    kotlin("jvm") version "1.3.61"
    id("application")
}

group = "pl.mrugacz95"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
}

tasks {
    compileKotlin {
        kotlinOptions.jvmTarget = "1.8"
    }
    compileTestKotlin {
        kotlinOptions.jvmTarget = "1.8"
    }
}
application {
    mainClassName = "pl.mrugacz95.day9.Day9Kt"
}