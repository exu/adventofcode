package task02

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

const in = `A Y
B X
C Z`

func TestResult2(t *testing.T) {
	d := Split2(in)

	sum := 0
	for _, e := range d {
		result := Result2(e.Shape, e.ExpectedResult)
		sum += result
	}

	assert.Equal(t, 12, sum)
}

func TestReal2(t *testing.T) {

	d := Split2(real)

	sum := 0
	for _, e := range d {
		result := Result2(e.Shape, e.ExpectedResult)
		sum += result
	}

	assert.Equal(t, 13193, sum)
}
