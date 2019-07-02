import json

res = {}
bench_name = []


def load_res(file):
    json_file = open(file)
    data = json.load(json_file)


    for bench in data[2]:
        if bench['reportName'] not in res:
             res[bench['reportName']] = [bench['reportAnalysis']['anRegress'][0]['regCoeffs']['iters']['estPoint']]
        else :
            res[bench['reportName']].append(bench['reportAnalysis']['anRegress'][0]['regCoeffs']['iters']['estPoint'])

        name = bench['reportName'].split('/')[1]

        if name not in bench_name:
            bench_name.append(name)

load_res('out10.json')
load_res('out50.json')
load_res('out100.json')

def formatt(time):
    if time >= 1:
        return ('%s s' % float('%.3g' % time))
    if time < 1 and time >= 1e-3:
        return ('%s ms' % float('%.3g' % (time*1e3)))
    elif time < 1e-3 and time >= 1e-6:
        return ('%s us' % float('%.3g' % (time*1e6)))
    elif time < 1e-6 and time >= 1e-9:
        return ('%s ns' % float('%.3g' % (time*1e9)))

for name in bench_name:
    print(name)
    print("| Library | n = 10 | n = 50 | n = 100 |")
    print("| --- | --- | --- | --- |")
    for bench in res.keys():
        if name == bench.split('/')[1]:
            lib = bench.split('/')[0]
            print('| ' + lib + ' | ' + formatt(res[bench][0]) + ' | ' + formatt(res[bench][1]) + ' | ' + formatt(res[bench][2]) + ' |' )

#'%s' % float('%.1g' % 1234)
