require("testing/lua/spec_helper")
require("hammerspoon/hammerspoon/config/framecache")
require("hammerspoon/hammerspoon/config/window")


describe("Window management", function()
    describe("computing the sign of a value", function()
        it("works for positive numbers", function()
            assert.are.equal(sign(1), 1)
        end)

        it("works for negative numbers", function()
            assert.are.equal(sign(-1), -1)
        end)
    end)

    describe("frame clamping", function()
        it("fits to the maximum with no frame border", function()
            local frame = { x = 10, y = 10, w = 1024, h = 10 }
            local max = { x = 0, y = 0, w = 100, h = 100 }
            local expected = { x = 10, y = 10, w = 100, h = 10 }

            local newFrame = clampFrame(frame, max, 0)

            assert.are.same(expected, newFrame)
        end)

        it("fits to the maximum with a frame border", function()
            local border = 16
            local frame = { x = 0, y = 0, w = 1024, h = 10 }
            local max = { x = 0, y = 0, w = 100, h = 100 }
            local expected = { x = 8, y = 8, w = 84, h = 10 }

            local newFrame = clampFrame(frame, max, border)

            assert.are.same(expected, newFrame)
        end)

        it("handles negative pixels", function()
            local border = 16
            local frame = { x = -1680, y = 0, w = 1024, h = 10 }
            local max = { x = 0, y = 0, w = 100, h = 100 }

            local expected = { x = 8, y = 8, w = 84, h = 10 }

            local newFrame = clampFrame(frame, max, border)

            assert.are.same(expected, newFrame)
        end)
    end)

    describe("maximising the window", function()
        it("maximises without border", function()
            local frame = { x = 10, y = 15, w = 20, h = 150 }
            local max = { x = 0, y = 0, w = 1024, h = 768 }
            local expected = max

            local newFrame = maximizeWindowSize(frame, max, 0)

            assert.are.same(expected, newFrame)
        end)

        it("maximises with border", function()
            local border = 16
            local frame = { x = 10, y = 15, w = 20, h = 150 }
            local max = { x = 0, y = 0, w = 1024, h = 768 }
            local expected = { x = 8, y = 8, w = 1008, h = 752 }

            local newFrame = maximizeWindowSize(frame, max, border)

            assert.are.same(expected, newFrame)
        end)

        it("maximises on a left screen", function()
            local border = 16
            local frame = { x = -300, y = 15, w = 20, h = 150 }
            local max = { x = -1920, y = 0, w = 1920, h = 1200 }
            local expected = { x = -1912, y = 8, w = 1904, h = 1184 }

            local newFrame = maximizeWindowSize(frame, max, border)

            assert.are.same(expected, newFrame)
        end)
    end)

    describe("moving the window to the left", function()
        it("moves the window to the left without border", function()
            local frame = { x = 10, y = 15, w = 20, h = 150 }
            local max = { x = 0, y = 0, w = 1024, h = 768 }
            local expected = { x = 0, y = 0, w = 512, h = 768 }

            local newFrame = windowLeftHalf(frame, max, 0)

            assert.are.same(expected, newFrame)
        end)

        it("moves the window to the left with border", function()
            local border = 16
            local frame = { x = 10, y = 15, w = 20, h = 150 }
            local max = { x = 0, y = 0, w = 1024, h = 768 }
            local expected = { x = 8, y = 8, w = 500, h = 752 }

            local newFrame = windowLeftHalf(frame, max, border)

            assert.are.same(expected, newFrame)
        end)

        it("moves the window to the left on a left screen", function()
            local border = 16
            local frame = { x = -100, y = 15, w = 20, h = 150 }
            local max = { x = -1680, y = 0, w = 1680, h = 1050 }
            local expected = { x = -1672, y = 8, w = 828, h = 1034 }

            local newFrame = windowLeftHalf(frame, max, border)

            assert.are.same(expected, newFrame)
        end)
    end)

    describe("moving the window to the right", function()
        it("moves the window to the right without border", function()
            local frame = { x = 10, y = 15, w = 20, h = 150 }
            local max = { x = 0, y = 0, w = 1024, h = 768 }
            local expected = { x = 512, y = 0, w = 512, h = 768 }

            local newFrame = windowRightHalf(frame, max, 0)

            assert.are.same(expected, newFrame)
        end)

        it("moves the window to the right with border", function()
            local border = 16
            local frame = { x = 10, y = 15, w = 20, h = 150 }
            local max = { x = 0, y = 0, w = 1024, h = 768 }
            local expected = { x = 520, y = 8, w = 500, h = 752 }

            local newFrame = windowRightHalf(frame, max, border)

            assert.are.same(expected, newFrame)
        end)

        it("moves the window to the right on a left screen", function()
            local border = 16
            local frame = { x = -100, y = 15, w = 20, h = 150 }
            local max = { x = -1680, y = 0, w = 1680, h = 1050 }
            local expected = { x = -832, y = 8, w = 828, h = 1034 }

            local newFrame = windowRightHalf(frame, max, border)

            assert.are.same(expected, newFrame)
        end)
    end)
end)
