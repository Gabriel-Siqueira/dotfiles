nmap <buffer> w 

function! s:get_id()
	let index = match(b:task_report_columns, '^uuid.*')
	let id = taskwarrior#data#get_value_by_index('.', index)
  if len(id) == 0
		let index = match(b:task_report_columns, '^id.*')
		let id = taskwarrior#data#get_value_by_index('.', index)
	endif
	return id
endfunction

function! TaskFile()
	let id = s:get_id()
  let list = systemlist('task _get ' . id . '.parent')
  if len(list) == 0
    let uuid = systemlist('task _uuid ' . id)[0]
  else
    let uuid = list[0]
  endif
  execute 'edit' $MY_WIKI . 'task/' . uuid . '.md'
endfunction
